const std = @import("std");

pub const Token = union(enum) {
    eof: []const u8,
    illegal: []const u8,
    ident: []const u8,
    int: []const u8,
    assign: []const u8,
    plus: []const u8,
    comma: []const u8,
    semicolon: []const u8,
    lparen: []const u8,
    rparen: []const u8,
    lbrace: []const u8,
    rbrace: []const u8,
    function: []const u8,
    let: []const u8,

    pub fn tokenName(self: *const Token) []const u8 {
        return @tagName(self.*);
    }

    pub fn literal(self: *const Token) []const u8 {
        switch (self.*) {
            .eof, .illegal, .ident, .int, .assign, .plus, .comma, .semicolon, .lparen, .rparen, .lbrace, .rbrace, .function, .let => |value| return value,
        }
    }
};
