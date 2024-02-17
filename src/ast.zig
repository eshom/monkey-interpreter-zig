const std = @import("std");
const token = @import("token.zig");
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

    pub fn tokenLiteral(self: *const Statement, allocator: Allocator) ![]const u8 {
        switch (self.*) {
            .let => |stm| return stm.token.literal(allocator),
        }
    }
};

pub const Expression = union(enum) {
    ident: Identifier,

    pub fn tokenLiteral(self: *const Expression, allocator: Allocator) ![]const u8 {
        switch (self.*) {
            .ident => |exp| return exp.token.literal(allocator),
        }
    }
};

pub const Program = struct {
    statements: ArrayListStmt,

    pub fn tokenLiteral(self: *const Program, allocator: Allocator) ![]const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral(allocator);
        } else {
            return "";
        }
    }

    pub fn new(allocator: Allocator) !*Program {
        const prog = try allocator.create(Program);
        prog.statements = ArrayListStmt.init(allocator);
        return prog;
    }

};

pub const Identifier = struct {
    token: token.Token, // token.Token.ident
    value: []const u8,
};

const LetStatement =  struct {
    token: token.Token, // token.Token.let
    name: *Identifier,
    value: ?Expression,
};
