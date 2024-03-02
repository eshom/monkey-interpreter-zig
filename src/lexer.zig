const std = @import("std");
const token = @import("token.zig");
const t = std.testing;
const Allocator = std.mem.Allocator;

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_pos: usize,
    ch: u8,

    pub fn new(allocator: Allocator, input: []const u8) !*Lexer {
        const out = try allocator.create(Lexer);
        out.* = .{ .input = input, .position = 0, .read_pos = 0, .ch = 0 };
        out.readChar();
        return out;
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

    fn peekChar(self: *Lexer) u8 {
        if (self.read_pos >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_pos];
        }
    }

    pub fn nextToken(self: *Lexer) token.Token {
        var tok: token.Token = undefined;

        while (std.ascii.isWhitespace(self.ch)) {
            self.readChar();
        }

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tok = @unionInit(token.Token, "eq", "==");
                } else {
                    tok = @unionInit(token.Token, "assign", "=");
                }
            },
            '!' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tok = @unionInit(token.Token, "not_eq", "!=");
                } else {
                    tok = @unionInit(token.Token, "bang", "!");
                }
            },
            '"' => {
                const literal = self.readString();
                tok = @unionInit(token.Token, "string", literal);
            },
            '+' => tok = @unionInit(token.Token, "plus", "+"),
            '-' => tok = @unionInit(token.Token, "minus", "-"),
            '/' => tok = @unionInit(token.Token, "slash", "/"),
            '*' => tok = @unionInit(token.Token, "asterisk", "*"),
            '<' => tok = @unionInit(token.Token, "lt", "<"),
            '>' => tok = @unionInit(token.Token, "gt", ">"),
            ';' => tok = @unionInit(token.Token, "semicolon", ";"),
            '(' => tok = @unionInit(token.Token, "lparen", "("),
            ')' => tok = @unionInit(token.Token, "rparen", ")"),
            ',' => tok = @unionInit(token.Token, "comma", ","),
            '{' => tok = @unionInit(token.Token, "lbrace", "{"),
            '}' => tok = @unionInit(token.Token, "rbrace", "}"),
            0 => tok = @unionInit(token.Token, "eof", "EOF"),
            else => {
                if (std.ascii.isAlphabetic(self.ch)) {
                    const literal = self.readIdentifier();
                    tok = token.lookupIdent(literal);
                    return tok;
                } else if (std.ascii.isDigit(self.ch)) {
                    const literal = self.readNumber();
                    if (std.mem.indexOfScalar(u8, literal, '.') != null) {
                        tok = @unionInit(token.Token, "float", literal);
                        return tok;
                    }
                    tok = @unionInit(token.Token, "int", literal);
                    return tok;
                } else {
                    tok = @unionInit(token.Token, "illegal", self.input[self.position..self.read_pos]);
                }
            },
        }
        self.readChar();
        return tok;
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        std.debug.assert(std.ascii.isAlphabetic(self.ch));
        const position = self.position;
        while (std.ascii.isAlphabetic(self.ch)) {
            self.readChar();
        }
        std.debug.assert(!std.ascii.isAlphabetic(self.ch));
        return self.input[position..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        std.debug.assert(std.ascii.isDigit(self.ch));
        const position = self.position;
        while (std.ascii.isDigit(self.ch)) {
            self.readChar();
        }

        if (self.ch == '.' and std.ascii.isDigit(self.peekChar())) {
            self.readChar();
            while (std.ascii.isDigit(self.ch)) {
                self.readChar();
            }
        }

        std.debug.assert(!std.ascii.isDigit(self.ch));
        return self.input[position..self.position];
    }

    fn readString(self: *Lexer) []const u8 {
        std.debug.assert(self.ch == '"');
        self.readChar();
        const position = self.position;
        while (self.ch != '"' and self.ch != 0) {
            self.readChar();
        }
        return self.input[position..self.position];
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

test "next token extended" {
    std.debug.print("\n", .{});

    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\  x + y;
        \\}
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\  return true;
        \\  } else {
        \\  return false;
        \\
        \\  10 == 10;
        \\  10 != 9;
        \\  5.0 < 9.99
        \\  5.
        \\  4.
        \\  let s = "hello";
        \\  let sx = "Hello, World!";
    ;
    const expected = [_]token.Token{
        .{ .let = "let" },
        .{ .ident = "five" },
        .{ .assign = "=" },
        .{ .int = "5" },
        .{ .semicolon = ";" },

        .{ .let = "let" },
        .{ .ident = "ten" },
        .{ .assign = "=" },
        .{ .int = "10" },
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
        .{ .asterisk = "*" },
        .{ .int = "5" },
        .{ .semicolon = ";" },

        .{ .int = "5" },
        .{ .lt = "<" },
        .{ .int = "10" },
        .{ .gt = ">" },
        .{ .int = "5" },
        .{ .semicolon = ";" },

        .{ .@"if" = "if" },
        .{ .lparen = "(" },
        .{ .int = "5" },
        .{ .lt = "<" },
        .{ .int = "10" },
        .{ .rparen = ")" },
        .{ .lbrace = "{" },
        .{ .@"return" = "return" },
        .{ .true = "true" },
        .{ .semicolon = ";" },
        .{ .rbrace = "}" },
        .{ .@"else" = "else" },
        .{ .lbrace = "{" },
        .{ .@"return" = "return" },
        .{ .false = "false" },
        .{ .semicolon = ";" },

        .{ .int = "10" },
        .{ .eq = "==" },
        .{ .int = "10" },
        .{ .semicolon = ";" },

        .{ .int = "10" },
        .{ .not_eq = "!=" },
        .{ .int = "9" },
        .{ .semicolon = ";" },

        .{ .float = "5.0" },
        .{ .lt = "<" },
        .{ .float = "9.99" },

        .{ .int = "5" },
        .{ .illegal = "." },

        .{ .int = "4" },
        .{ .illegal = "." },

        .{ .let = "let" },
        .{ .ident = "s" },
        .{ .assign = "=" },
        .{ .string = "hello" },
        .{ .semicolon = ";" },

        .{ .let = "let" },
        .{ .ident = "sx" },
        .{ .assign = "=" },
        .{ .string = "Hello, World!" },
        .{ .semicolon = ";" },

        .{ .eof = "EOF" },
    };

    const lex = try Lexer.new(t.allocator, input);
    defer t.allocator.destroy(lex);

    for (expected) |ex| {
        const tok = lex.nextToken();
        std.debug.print("ex: {s:<10} -> {s:<10} found: {s:<10} -> {s:<10}\n", .{ ex.tokenName(), ex.literal(), tok.tokenName(), tok.literal() });
        try t.expectEqualDeep(ex, tok);
    }
}
