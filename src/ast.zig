const std = @import("std");
const token = @import("token.zig");
const Allocator = std.mem.Allocator;

pub const Statement = union(enum) {
    let: LetStatement,
    @"return": ReturnStatement,

    pub fn literal(self: *const Statement) []const u8 {
        switch (self.*) {
            inline else => |value| return value.literal(),
        }
    }

    pub fn tag(self: *const Statement) []const u8 {
        return @tagName(self.*);
    }
};

pub const Expression = union(enum) {
    ident: Identifier,

    pub fn literal(self: *const Expression) []const u8 {
        switch (self.*) {
            inline else => |value| return value.literal(),
        }
    }

    pub fn tag(self: *const Expression) []const u8 {
        return @tagName(self.*);
    }
};

pub const Program = struct {
    statements: std.ArrayList(*Statement),
    allocator: Allocator,

    pub fn init(allocator: Allocator) !*Program {
        const prog = try allocator.create(Program);
        errdefer allocator.destroy(prog);

        prog.* = Program{
            .statements = std.ArrayList(*Statement).init(allocator),
            .allocator = allocator,
        };

        return prog;
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
        self.allocator.destroy(self);
    }

    pub fn literal(self: *const Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].literal();
        } else {
            return "";
        }
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: *Identifier,
    value: *Expression,

    pub fn literal(self: *const LetStatement) []const u8 {
        return self.token.literal();
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn literal(self: *const Identifier) []const u8 {
        return self.token.literal();
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    return_value: *Expression,

    pub fn literal(self: *const ReturnStatement) []const u8 {
        return self.token.literal();
    }
};
