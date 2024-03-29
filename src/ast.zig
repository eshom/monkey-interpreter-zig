const std = @import("std");
const t = std.testing;
const token = @import("token.zig");
//const lexer = @import("lexer.zig");
//const parser = @import("parser.zig");
const ArrayListStmt = std.ArrayList(Statement);
const ArrayListIdent = std.ArrayList(Identifier);
const ArrayListExpr = std.ArrayList(Expression);
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
    block: BlockStatement,

    pub fn tokenLiteral(self: *const Statement, allocator: Allocator) ![]const u8 {
        switch (self.*) {
            .let, .@"return", .expression, .block => |stm| return stm.token.literal(allocator),
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
            .block => |stmt| {
                return try stmt.string(allocator);
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
    @"if": IfExpression,
    function: FunctionLiteral,
    call: CallExpression,

    pub fn tokenLiteral(self: *const Expression, allocator: Allocator) ![]const u8 {
        switch (self.*) {
            .ident, .int, .prefix, .infix, .boolean, .@"if", .function, .call => |exp| return try exp.token.literal(allocator),
        }
    }

    pub fn string(self: *const Expression, allocator: Allocator) @typeInfo(@typeInfo(@TypeOf(Statement.string)).Fn.return_type.?).ErrorUnion.error_set![]const u8 {
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
            .@"if" => |exp| {
                return try std.fmt.allocPrint(allocator, "if {s} {s}{s}", .{
                    try exp.condition.string(allocator),
                    try exp.consequence.string(allocator),
                    if (exp.alternative) |alt| try std.mem.join(allocator, " ", &[_][]const u8{ " else", try alt.string(allocator) }) else "",
                });
            },
            .function => |exp| {
                const params_str = try allocator.alloc([]const u8, exp.parameters.items.len);
                errdefer allocator.free(params_str);

                for (exp.parameters.items, params_str) |param, *str| {
                    str.* = param.value;
                }

                const params_join = try std.mem.join(allocator, ", ", params_str);
                errdefer allocator.free(params_join);

                return try std.fmt.allocPrint(allocator, "{s}({s}) {s}", .{
                    try exp.token.literal(allocator),
                    params_join,
                    try exp.body.string(allocator),
                });
            },
            .call => |exp| {
                const args_str = try allocator.alloc([]const u8, exp.arguments.items.len);
                errdefer allocator.free(args_str);

                for (exp.arguments.items, args_str) |arg, *str| {
                    str.* = try arg.string(allocator);
                }

                return try std.fmt.allocPrint(allocator, "{s}({s})", .{
                    if (exp.function) |func| try func.string(allocator) else "",
                    try std.mem.join(allocator, ", ", args_str),
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

pub const IfExpression = struct {
    token: token.Token, // token.Token.if
    condition: *Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,
};

pub const BlockStatement = struct {
    token: token.Token, // token.Token.lbrace
    statements: ArrayListStmt,

    pub fn new(allocator: Allocator) !*BlockStatement {
        const block = try allocator.create(BlockStatement);
        block.statements = ArrayListStmt.init(allocator);
        block.token = .{ .illegal = "unititialized block statement" }; // should be set by the caller
        return block;
    }

    pub fn string(self: *const BlockStatement, allocator: Allocator) @typeInfo(@typeInfo(@TypeOf(Statement.string)).Fn.return_type.?).ErrorUnion.error_set![]const u8 {
        const strs = try allocator.alloc([]const u8, self.statements.items.len * 2 + 2); // times 2 for semicolons + 2 for braces
        errdefer allocator.free(strs);

        strs[0] = "{ ";
        var strs_idx: usize = 1;
        var stmt_idx: usize = 0;
        while (stmt_idx < self.statements.items.len) : ({
            strs_idx += 2;
            stmt_idx += 1;
        }) {
            strs[strs_idx] = try self.statements.items[stmt_idx].string(allocator);
            strs[strs_idx + 1] = "; ";
        }
        strs[strs.len - 1] = "}";

        return try std.mem.join(allocator, "", strs);
    }
};

pub const FunctionLiteral = struct {
    token: token.Token, // token.Token.function
    parameters: ArrayListIdent,
    body: *BlockStatement,

    pub fn new(allocator: Allocator) !*FunctionLiteral {
        const fnlit = try allocator.create(FunctionLiteral);
        errdefer allocator.destroy(fnlit);

        fnlit.parameters = ArrayListIdent.init(allocator);
        errdefer fnlit.parameters.deinit();

        fnlit.body = try BlockStatement.new(allocator);
        errdefer allocator.destroy(fnlit.body);
        errdefer fnlit.body.deinit();

        fnlit.token = .{ .illegal = "unititialized function literal" }; // should be set by the caller

        return fnlit;
    }
};

pub const CallExpression = struct {
    token: token.Token, // token.Token.lparen
    function: ?*Expression, // Identifier or FunctionLiteral
    arguments: ArrayListExpr,

    pub fn new(allocator: Allocator) !*CallExpression {
        const call = try allocator.create(CallExpression);
        errdefer allocator.destroy(call);

        call.function = null;

        call.arguments = ArrayListExpr.init(allocator);
        errdefer call.arguments.deinit();

        call.token = .{ .illegal = "unititialized call expression" }; // should be set by the caller

        return call;
    }
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
