const std = @import("std");
const token = @import("token.zig");
const t = std.testing;
const Allocator = std.mem.Allocator;

const Lexer = struct {
    input: []const u8,
    position: usize,
    readPosition: usize,
    ch: u8,

    pub fn new(allocator: Allocator, input: []const u8) !*Lexer {
        const out = try allocator.create(Lexer);
        out.* = .{ .input = input, .position = 0, .readPosition = 0, .ch = 0 };
        out.readChar();
        return out;
    }

    fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }
        self.position = self.readPosition;

        self.readPosition += 1;
    }

    pub fn nextToken(self: *Lexer) token.Token {
        var tok: token.Token = undefined;

        switch (self.ch) {
            '=' => tok = token.Token{ .assign = "=" },
            ';' => tok = token.Token{ .semicolon = ";" },
            '(' => tok = token.Token{ .lparen = "(" },
            ')' => tok = token.Token{ .rparen = ")" },
            ',' => tok = token.Token{ .comma = "," },
            '+' => tok = token.Token{ .plus = "+" },
            '{' => tok = token.Token{ .lbrace = "{" },
            '}' => tok = token.Token{ .rbrace = "}" },
            0 => tok = token.Token{ .eof = "EOF" },
            else => {
                tok = token.Token{ .illegal = "ILLEGAL" };
            },
        }
        self.readChar();
        return tok;
    }
};

test "next token" {
    std.debug.print("\n", .{});

    const input = "=+(){},;";
    const expected = [_]token.Token{
        .{ .assign = "=" },
        .{ .plus = "+" },
        .{ .lparen = "(" },
        .{ .rparen = ")" },
        .{ .lbrace = "{" },
        .{ .rbrace = "}" },
        .{ .comma = "," },
        .{ .semicolon = ";" },
        .{ .eof = "EOF" },
    };

    const lex = try Lexer.new(t.allocator, input);
    defer t.allocator.destroy(lex);

    for (expected) |ex| {
        const tok = lex.nextToken();
        std.debug.print("ex: {s:<10} -> {s:<10} found: {s:<10} -> {s:<10}\n", .{ ex.tokenName(), ex.literal(), tok.tokenName(), tok.literal() });
        try t.expectEqual(ex, tok);
    }
}
