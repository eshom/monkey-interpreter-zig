const std = @import("std");
const token = @import("token.zig");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const Allocator = std.mem.Allocator;
const t = std.testing;

const ParserError = error{ UnexpectedToken, UnhandeledToken, ParsingError };

pub const Parser = struct {
    lex: *lexer.Lexer,
    cur_tok: token.Token,
    peek_tok: token.Token,
    allocator: Allocator,
    arena: std.heap.ArenaAllocator, // for expressions and sub-statements
    allocator_arena: Allocator, // Makes it easier due to recursive nature of the parser
    errors: std.ArrayList([]const u8),

    pub fn init(allocator: Allocator, lex: *lexer.Lexer) !*Parser {
        var par = try allocator.create(Parser);

        par.* = Parser{
            .allocator = allocator,
            .allocator_arena = undefined,
            .arena = undefined,
            .lex = lex,
            .cur_tok = undefined,
            .peek_tok = undefined,
            .errors = std.ArrayList([]const u8).init(allocator),
        };

        par.arena = std.heap.ArenaAllocator.init(allocator);
        par.allocator_arena = par.arena.allocator();

        // sets current token and peek token
        par.nextToken();
        par.nextToken();

        return par;
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
        for (self.errors.items) |err| {
            self.allocator.free(err);
        }
        self.errors.deinit();
        self.allocator.destroy(self);
    }

    fn nextToken(self: *Parser) void {
        self.cur_tok = self.peek_tok;
        self.peek_tok = self.lex.nextToken();
    }

    // parser owns memory for all ast, caller responsible to deinit
    pub fn parseProgram(self: *Parser) !*ast.Program {
        var prog = try ast.Program.init(self.allocator_arena);
        errdefer prog.deinit();

        while (self.cur_tok != .eof) {
            defer self.nextToken();

            const stmt = self.parseStatement() catch |err| switch (err) {
                ParserError.UnexpectedToken, ParserError.UnhandeledToken => {
                    // parser errors are recorded in Parser.errors as they happen
                    continue;
                },
                else => return err, // unexpected errors (e.g. OutOfMemory)
            };
            try prog.statements.append(stmt);
        }

        return prog;
    }

    fn parseStatement(self: *Parser) !*ast.Statement {
        switch (self.cur_tok) {
            .let => return self.parseLetStatement(),
            .@"return" => return self.parseReturnStatement(),
            else => {
                const msg = try std.fmt.allocPrint(self.allocator, "{any}: token `{s}` is not handeled", .{ ParserError.UnhandeledToken, self.cur_tok.tokenName() });
                self.errors.append(msg) catch |err| {
                    self.allocator.free(msg);
                    return err;
                };
                return ParserError.UnhandeledToken;
            },
        }
    }

    fn parseLetStatement(self: *Parser) !*ast.Statement {
        const stmt = try self.allocator_arena.create(ast.Statement);
        errdefer self.allocator_arena.destroy(stmt);

        const tok = self.cur_tok;

        std.debug.assert(tok == .let);

        if (!self.expectPeek(.ident)) {
            return self.peekError("ident");
        }

        const ident = try self.allocator_arena.create(ast.Identifier);
        errdefer self.allocator_arena.destroy(ident);
        ident.* = ast.Identifier{
            .token = self.cur_tok,
            .value = self.cur_tok.literal(),
        };

        if (!self.expectPeek(.assign)) {
            return self.peekError("assign");
        }

        // TODO: change to actual expression parsing
        // Currently just skipping until semicolon
        while (self.cur_tok != .semicolon) {
            self.nextToken();
        }

        const exp = try self.allocator_arena.create(ast.Expression);
        errdefer self.allocator_arena.destroy(exp);

        exp.* = ast.Expression{
            .ident = ident.*,
        };

        // ident should have been copied to prevent double free
        std.debug.assert(ident != &exp.ident);

        stmt.* = ast.Statement{
            .let = .{
                .token = tok,
                .name = ident,
                .value = exp,
            },
        };

        return stmt;
    }

    fn curTokenIs(self: *Parser, tok: token.TokenTag) bool {
        return self.cur_tok.tag() == tok;
    }

    fn peekTokenIs(self: *Parser, tok: token.TokenTag) bool {
        return self.peek_tok.tag() == tok;
    }

    fn expectPeek(self: *Parser, tok: token.TokenTag) bool {
        if (self.peekTokenIs(tok)) {
            self.nextToken();
            return true;
        } else {
            return false;
        }
    }

    fn peekError(self: *Parser, tok_name: []const u8) anyerror {
        const msg = try std.fmt.allocPrint(self.allocator, "{any}: expected next token to be `{s}`, got `{s}` instead", .{ ParserError.UnexpectedToken, tok_name, self.peek_tok.tokenName() });
        self.errors.append(msg) catch |err| {
            self.allocator.free(msg);
            return err;
        };
        return ParserError.UnexpectedToken;
    }

    fn checkParserErrors(self: *Parser) !void {
        if (self.errors.items.len == 0) {
            return;
        }

        const stderr = std.io.getStdErr();
        var writer = stderr.writer();

        try writer.print("parser has {d} errors\n", .{self.errors.items.len});
        for (self.errors.items) |err| {
            try writer.print("{s}\n", .{err});
        }

        return ParserError.ParsingError;
    }

    fn parseReturnStatement(self: *Parser) !*ast.Statement {
        const stmt = try self.allocator_arena.create(ast.Statement);
        errdefer self.allocator_arena.destroy(stmt);

        const tok = self.cur_tok;

        std.debug.assert(tok == .@"return");

        self.nextToken();

        // TODO: change to actual expression ParsingError
        const exp = try self.allocator_arena.create(ast.Expression);
        errdefer self.allocator_arena.destroy(exp);

        exp.* = ast.Expression{
            .ident = ast.Identifier{
                .token = self.cur_tok,
                .value = self.cur_tok.literal(),
            },
        };

        stmt.* = ast.Statement{
            .@"return" = .{
                .token = tok,
                .return_value = exp,
            },
        };

        while (self.cur_tok != .semicolon) {
            self.nextToken();
        }

        return stmt;
    }
};

test "let statements" {
    std.debug.print("\n", .{});

    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
        \\let 5;
    ;

    const lex = try lexer.Lexer.init(t.allocator, input);
    defer lex.deinit();

    const par = try Parser.init(t.allocator, lex);
    defer par.deinit();

    const prog = try par.parseProgram();
    defer prog.deinit();

    const parse_err = par.checkParserErrors();
    try t.expectError(ParserError.ParsingError, parse_err);
    try t.expectStringStartsWith(par.errors.items[0], "error.UnexpectedToken");

    try t.expectEqual(3, prog.statements.items.len);

    const expected_idents = [_][]const u8{
        "x", "y", "foobar",
    };

    for (expected_idents, prog.statements.items) |expected, stmt| {
        try t.expectEqualStrings("let", stmt.literal());
        try t.expectEqualStrings("let", stmt.tag());
        try t.expectEqualStrings(expected, stmt.let.name.value);
    }
}

test "return statements" {
    std.debug.print("\n", .{});

    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    const lex = try lexer.Lexer.init(t.allocator, input);
    defer lex.deinit();

    const par = try Parser.init(t.allocator, lex);
    defer par.deinit();

    const prog = try par.parseProgram();
    defer prog.deinit();

    try par.checkParserErrors();

    try t.expectEqual(3, prog.statements.items.len);

    // TODO: add expression parsing and check for expression
    for (prog.statements.items) |stmt| {
        try t.expectEqualStrings("return", stmt.literal());
        try t.expectEqualStrings("return", stmt.tag());
    }
}
