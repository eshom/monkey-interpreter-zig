const std = @import("std");

pub const Token = union(enum) {
    illegal: []const u8,
    eof: []const u8,

    ident: []const u8,
    int: u64,

    assign: []const u8,
    plus: []const u8,
    minus: []const u8,
    bang: []const u8,
    asterix: []const u8,
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
    @"true": bool,
    @"false": bool,
    @"if": []const u8,
    @"else": []const u8,
    @"return": []const u8,

    pub fn print(self: *const Token, out: std.fs.File.Writer) !void {
        switch (self.*) {
            .illegal, .eof, .ident, .assign, .plus, .minus, .bang, .asterix, .slash,
            .lt, .gt, .eq, .not_eq, .comma, .semicolon, .lparen, .rparen, .lbrace, .rbrace,
            .function, .let, .@"if", .@"else",
            .@"return" => |v| try out.print("Type: {s:<10} || Literal: {s}\n", .{@tagName(self.*), v}),
            .int => |v| try out.print("Type: {s:<10} || Literal: {d}\n", .{@tagName(self.*), v}),
            .true, .false => |v| try out.print("Type: {s:<10} || Literal: {any}\n", .{@tagName(self.*), v}),
        }
    }
};

const keywords = std.comptime_string_map.ComptimeStringMap(Token, .{
    .{"fn", Token{.function = "fn"}},
    .{"let", Token{.let = "let"}},
    .{"true", Token{.@"true" = true}},
    .{"false", Token{.@"false" = false}},
    .{"if", Token{.@"if" = "if"}},
    .{"else", Token{.@"else" = "else"}},
    .{"return", Token{.@"return" = "return"}},
});

pub fn lookupIdent(ident: []const u8) Token {
    return keywords.get(ident) orelse Token{.ident = ident};
}
