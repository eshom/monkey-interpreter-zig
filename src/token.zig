const std = @import("std");

pub const Token = union(enum) {
    eof: []const u8,
    illegal: []const u8,
    ident: []const u8,
    int: []const u8,
    float: []const u8,
    assign: []const u8,
    plus: []const u8,
    minus: []const u8,
    bang: []const u8,
    asterisk: []const u8,
    slash: []const u8,
    lt: []const u8,
    gt: []const u8,
    eq: []const u8,
    not_eq: []const u8,
    comma: []const u8,
    semicolon: []const u8,
    lparen: []const u8,
    rparen: []const u8,
    lbrace: []const u8,
    rbrace: []const u8,
    function: []const u8,
    let: []const u8,
    true: []const u8,
    false: []const u8,
    @"if": []const u8,
    @"else": []const u8,
    @"return": []const u8,
    string: []const u8,

    pub fn tokenName(self: *const Token) []const u8 {
        return @tagName(self.*);
    }

    pub fn literal(self: *const Token) []const u8 {
        switch (self.*) {
            inline else => |value| return value,
        }
    }

    pub fn print(self: *const Token, out: std.fs.File.Writer) !void {
        switch (self.*) {
            inline else => |value| try out.print("token: {s:<10} -> literal: {s}\n", .{ self.tokenName(), value }),
        }
    }
};

pub const TokenTag = std.meta.Tag(Token);

const keywords = std.comptime_string_map.ComptimeStringMap(Token, .{
    .{ "fn", @unionInit(Token, "function", "fn") },
    .{ "let", @unionInit(Token, "let", "let") },
    .{ "true", @unionInit(Token, "true", "true") },
    .{ "false", @unionInit(Token, "false", "false") },
    .{ "if", @unionInit(Token, "if", "if") },
    .{ "else", @unionInit(Token, "else", "else") },
    .{ "return", @unionInit(Token, "return", "return") },
});

pub fn lookupIdent(ident: []const u8) Token {
    return keywords.get(ident) orelse .{ .ident = ident };
}
