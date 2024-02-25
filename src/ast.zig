const std = @import("std");
const t = std.testing;
const token = @import("token.zig");
//const lexer = @import("lexer.zig");
//const parser = @import("parser.zig");
const ArrayListStmt = std.ArrayList(Statement);
const Allocator = std.mem.Allocator;

//          Node
// Statement    Expression
// let          ident

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
};

pub const Statement = union(enum) {
    let: LetStatement,
    @"return": ReturnStatement,
    expression: ExpressionStatement,

    pub fn tokenLiteral(self: *const Statement, allocator: Allocator) ![]const u8 {
        switch (self.*) {
            .let, .@"return", .expression => |stm| return stm.token.literal(allocator),
        }
    }

    pub fn string(self: *const Statement, allocator: Allocator) ![]const u8 {
        switch (self.*) {
            .let => |stmt| {
                return try std.fmt.allocPrint(allocator, "{s} {s} = {s};", .{
                    try stmt.token.literal(allocator),
                    stmt.name.value,
                    if (stmt.value) |st| try st.string(allocator) else "",
                });
            },
            .@"return" => |stmt| {
                return try std.fmt.allocPrint(allocator, "{s} {s};", .{
                    try stmt.token.literal(allocator),
                    if (stmt.return_value) |val| try val.string(allocator) else "",
                });
            },
            .expression => |stmt| {
                return if (stmt.expression) |exp| exp.string(allocator) else "";
            },
        }
    }
};

pub const Expression = union(enum) {
    ident: Identifier,
    int: IntegerLiteral,
    prefix: PrefixExpression,
    infix: InfixExpression,
    boolean: Boolean,

    pub fn tokenLiteral(self: *const Expression, allocator: Allocator) ![]const u8 {
        switch (self.*) {
            .ident, .int, .prefix, .infix, .boolean => |exp| return try exp.token.literal(allocator),
        }
    }

    pub fn string(self: *const Expression, allocator: Allocator) ![]const u8 {
        switch (self.*) {
            .ident => |exp| return exp.value,
            .int => |exp| return try exp.token.literal(allocator),
            .boolean => |exp| return try exp.token.literal(allocator),
            .prefix => |exp| {
                return try std.fmt.allocPrint(allocator, "({s}{s})", .{
                    exp.op,
                    if (exp.right) |right| try right.string(allocator) else "",
                });
            },
            .infix => |exp| {
                return try std.fmt.allocPrint(allocator, "({s} {s} {s})", .{
                    if (exp.left) |left| try left.string(allocator) else "",
                    exp.op,
                    if (exp.right) |right| try right.string(allocator) else "",
                });
            },
        }
    }
};

pub const Program = struct {
    statements: ArrayListStmt,

    pub fn tokenLiteral(self: *const Program, allocator: Allocator) ![]const u8 {
        if (self.statements.items.len > 0) {
            return try self.statements.items[0].tokenLiteral(allocator);
        } else {
            return "";
        }
    }

    pub fn new(allocator: Allocator) !*Program {
        const prog = try allocator.create(Program);
        prog.statements = ArrayListStmt.init(allocator);
        return prog;
    }

    pub fn string(self: *Program, allocator: Allocator) ![]const u8 {
        var out = std.ArrayList([]const u8).init(allocator);
        defer out.deinit();
        for (self.statements.items) |stmt| {
            try out.append(try stmt.string(allocator));
        }

        return try std.mem.join(allocator, "\n", out.items);
    }

    pub fn prettyPrint(str: []const u8, writer: std.fs.File.Writer) !void {
        var indent: usize = 0;

        _ = try writer.write("####");
        try writer.writeByte('\n');
        for (str) |c| {
            if (c == '(') {
                try writer.writeByte('(');
                for (0..indent + 2) |_| try writer.writeByte(' ');
                try writer.writeByte('\n');
                for (0..indent + 2) |_| try writer.writeByte(' ');
                indent += 2;
            } else if (c == ')') {
                indent -= 2;
                try writer.writeByte('\n');
                for (0..indent) |_| try writer.writeByte(' ');
                try writer.writeByte(')');
            } else if (c == '\n') {
                try writer.writeByte('\n');
                _ = try writer.write("####");
                try writer.writeByte('\n');
            } else {
                try writer.writeByte(c);
            }
        }
    }
};

pub const Identifier = struct {
    token: token.Token, // token.Token.ident
    value: []const u8,
};

pub const LetStatement = struct {
    token: token.Token, // token.Token.let
    name: *Identifier,
    value: ?Expression,
};

pub const ReturnStatement = struct {
    token: token.Token, // token.Token.return
    return_value: ?Expression,
};

pub const ExpressionStatement = struct {
    token: token.Token, // the first token of the expression
    expression: ?Expression,
};

pub const IntegerLiteral = struct {
    token: token.Token, // token.Token.int
    value: i64,
};

pub const PrefixExpression = struct {
    token: token.Token, // the prefix token, e.g. !
    op: []const u8,
    right: ?*Expression,
};

pub const InfixExpression = struct {
    token: token.Token, // the infix token, e.g. +
    left: ?*Expression,
    op: []const u8,
    right: ?*Expression,
};

pub const Boolean = struct {
    token: token.Token,
    value: bool,
};

test "stringify statement" {
    std.debug.print("\n", .{});

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const let_stmt = try allocator.create(Statement);
    const ident = try allocator.create(Identifier);

    ident.* = .{
        .token = .{ .ident = "myVar" },
        .value = "myVar",
    };

    let_stmt.* = .{ .let = .{ .token = .{ .let = "let" }, .name = ident, .value = .{ .ident = .{
        .token = .{ .ident = "anotherVar" },
        .value = "anotherVar",
    } } } };

    var program = try Program.new(allocator);
    try program.statements.append(let_stmt.*);

    std.debug.print("{s}\n", .{try program.string(allocator)});
    try t.expectEqualStrings(try program.string(allocator), "let myVar = anotherVar;");
}
