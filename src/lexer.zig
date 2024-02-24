const std = @import("std");
const t = std.testing;
const token = @import("token.zig");
const Allocator = std.mem.Allocator;

pub const Lexer = struct {
    input: []const u8,
    position: usize, // current position in char
    read_pos: usize, // current read position after current char
    ch: u8 = 0, // current char being examined

    pub fn new(allocator: Allocator, input: []const u8) !*Lexer {
        const lex = try allocator.create(Lexer);
        lex.input = input;
        lex.position = 0;
        lex.read_pos = 0;
        lex.readChar();
        return lex;
    }

    fn readChar(self: *Lexer) void {
        if (self.read_pos >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
        }
        self.position = self.read_pos;
        self.read_pos += 1;
    }

    pub fn nextToken(self: *Lexer) token.Token {
        var tok: token.Token = undefined;

        self.skipWS();

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    tok = .{ .eq = self.input[self.position .. self.read_pos + 1] };
                    self.readChar();
                } else {
                    tok = .{ .assign = self.input[self.position..self.read_pos] };
                }
            },
            '!' => {
                if (self.peekChar() == '=') {
                    tok = .{ .not_eq = self.input[self.position .. self.read_pos + 1] };
                    self.readChar();
                } else {
                    tok = .{ .bang = self.input[self.position..self.read_pos] };
                }
            },
            ';' => tok = .{ .semicolon = self.input[self.position..self.read_pos] },
            '(' => tok = .{ .lparen = self.input[self.position..self.read_pos] },
            ')' => tok = .{ .rparen = self.input[self.position..self.read_pos] },
            ',' => tok = .{ .comma = self.input[self.position..self.read_pos] },
            '+' => tok = .{ .plus = self.input[self.position..self.read_pos] },
            '{' => tok = .{ .lbrace = self.input[self.position..self.read_pos] },
            '}' => tok = .{ .rbrace = self.input[self.position..self.read_pos] },
            '-' => tok = .{ .minus = self.input[self.position..self.read_pos] },
            '*' => tok = .{ .asterix = self.input[self.position..self.read_pos] },
            '/' => tok = .{ .slash = self.input[self.position..self.read_pos] },
            '<' => tok = .{ .lt = self.input[self.position..self.read_pos] },
            '>' => tok = .{ .gt = self.input[self.position..self.read_pos] },

            0 => tok = .{ .eof = "" },
            else => {
                if (std.ascii.isAlphabetic(self.ch)) {
                    tok = token.lookupIdent(self.readIdent());
                } else if (std.ascii.isDigit(self.ch)) {
                    const num = self.readNumber();
                    if (num) |n| {
                        tok = .{ .int = n };
                    } else {
                        tok = .{ .illegal = "illegal number" };
                    }
                } else {
                    tok = .{ .illegal = self.input[self.position..self.read_pos] };
                }
            },
        }

        self.readChar();
        return tok;
    }

    fn readIdent(self: *Lexer) []const u8 {
        const pos = self.position;

        while (std.ascii.isAlphabetic(self.ch) and std.ascii.isAlphabetic(self.peekChar())) {
            self.readChar();
        }

        return self.input[pos..self.read_pos];
    }

    fn skipWS(self: *Lexer) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
    }

    fn readNumber(self: *Lexer) ?i64 {
        const pos = self.position;

        while (std.ascii.isDigit(self.ch) and std.ascii.isDigit(self.peekChar())) {
            self.readChar();
        }

        const num_str = self.input[pos..self.read_pos];
        return std.fmt.parseInt(i64, num_str, 10) catch return null;
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_pos >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_pos];
        }
    }
};

test "next token" {
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
    };

    var lex = try Lexer.new(t.allocator, input);
    defer t.allocator.destroy(lex);

    for (expected) |exp_tok| {
        const tok = lex.nextToken();
        try t.expectEqualDeep(exp_tok, tok);
    }
}

test "next token real code" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;

    const expected = [_]token.Token{
        .{ .let = "let" },
        .{ .ident = "five" },
        .{ .assign = "=" },
        .{ .int = 5 },
        .{ .semicolon = ";" },
        .{ .let = "let" },
        .{ .ident = "ten" },
        .{ .assign = "=" },
        .{ .int = 10 },
        .{ .semicolon = ";" },
        .{ .let = "let" },
        .{ .ident = "add" },
        .{ .assign = "=" },
        .{ .function = "fn" },
        .{ .lparen = "(" },
        .{ .ident = "x" },
        .{ .comma = "," },
        .{ .ident = "y" },
        .{ .rparen = ")" },
        .{ .lbrace = "{" },
        .{ .ident = "x" },
        .{ .plus = "+" },
        .{ .ident = "y" },
        .{ .semicolon = ";" },
        .{ .rbrace = "}" },
        .{ .semicolon = ";" },
        .{ .let = "let" },
        .{ .ident = "result" },
        .{ .assign = "=" },
        .{ .ident = "add" },
        .{ .lparen = "(" },
        .{ .ident = "five" },
        .{ .comma = "," },
        .{ .ident = "ten" },
        .{ .rparen = ")" },
        .{ .semicolon = ";" },
    };

    var lex = try Lexer.new(t.allocator, input);
    defer t.allocator.destroy(lex);

    for (expected) |exp_tok| {
        const tok = lex.nextToken();
        //std.debug.print("Expected: {any}, Got: {any}\n", .{exp_tok, tok});
        try t.expectEqualDeep(exp_tok, tok);
    }
}

test "next token extended" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
        \\
    ;

    const expected = [_]token.Token{
        .{ .let = "let" },
        .{ .ident = "five" },
        .{ .assign = "=" },
        .{ .int = 5 },
        .{ .semicolon = ";" },
        .{ .let = "let" },
        .{ .ident = "ten" },
        .{ .assign = "=" },
        .{ .int = 10 },
        .{ .semicolon = ";" },
        .{ .let = "let" },
        .{ .ident = "add" },
        .{ .assign = "=" },
        .{ .function = "fn" },
        .{ .lparen = "(" },
        .{ .ident = "x" },
        .{ .comma = "," },
        .{ .ident = "y" },
        .{ .rparen = ")" },
        .{ .lbrace = "{" },
        .{ .ident = "x" },
        .{ .plus = "+" },
        .{ .ident = "y" },
        .{ .semicolon = ";" },
        .{ .rbrace = "}" },
        .{ .semicolon = ";" },
        .{ .let = "let" },
        .{ .ident = "result" },
        .{ .assign = "=" },
        .{ .ident = "add" },
        .{ .lparen = "(" },
        .{ .ident = "five" },
        .{ .comma = "," },
        .{ .ident = "ten" },
        .{ .rparen = ")" },
        .{ .semicolon = ";" },

        .{ .bang = "!" },
        .{ .minus = "-" },
        .{ .slash = "/" },
        .{ .asterix = "*" },
        .{ .int = 5 },
        .{ .semicolon = ";" },

        .{ .int = 5 },
        .{ .lt = "<" },
        .{ .int = 10 },
        .{ .gt = ">" },
        .{ .int = 5 },
        .{ .semicolon = ";" },

        .{ .@"if" = "if" },
        .{ .lparen = "(" },
        .{ .int = 5 },
        .{ .lt = "<" },
        .{ .int = 10 },
        .{ .rparen = ")" },
        .{ .lbrace = "{" },
        .{ .@"return" = "return" },
        .{ .true = true },
        .{ .semicolon = ";" },
        .{ .rbrace = "}" },
        .{ .@"else" = "else" },
        .{ .lbrace = "{" },
        .{ .@"return" = "return" },
        .{ .false = false },
        .{ .semicolon = ";" },
        .{ .rbrace = "}" },

        .{ .int = 10 },
        .{ .eq = "==" },
        .{ .int = 10 },
        .{ .semicolon = ";" },
        .{ .int = 10 },
        .{ .not_eq = "!=" },
        .{ .int = 9 },
        .{ .semicolon = ";" },
    };

    var lex = try Lexer.new(t.allocator, input);
    defer t.allocator.destroy(lex);

    for (expected) |exp_tok| {
        const tok = lex.nextToken();
        //std.debug.print("Expected: {any}, Got: {any}\n", .{exp_tok, tok});
        try t.expectEqualDeep(exp_tok, tok);
    }
}
