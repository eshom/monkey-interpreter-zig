const std = @import("std");
const token = @import("token.zig");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const Allocator = std.mem.Allocator;
const t = std.testing;

pub const Parser = struct {
    lex: *lexer.Lexer,
    cur_tok: token.Token,
    peek_tok: token.Token,
    allocator: Allocator,
    arena: std.heap.ArenaAllocator, // for expressions and sub-statements
    allocator_arena: Allocator, // Makes it easier due to recursive nature of the parser

    pub fn init(allocator: Allocator, lex: *lexer.Lexer) !*Parser {
        var par = try allocator.create(Parser);

        par.* = Parser{
            .allocator = allocator,
            .allocator_arena = undefined,
            .arena = undefined,
            .lex = lex,
            .cur_tok = undefined,
            .peek_tok = undefined,
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
        self.allocator.destroy(self);
    }

    fn nextToken(self: *Parser) void {
        self.cur_tok = self.peek_tok;
        self.peek_tok = self.lex.nextToken();
    }

    // program owns memory for statements but caller must deinit program
    pub fn parseProgram(self: *Parser) !*ast.Program {
        // program and parser share the same underlying allocator
        // however program owns all the top level statements, parser everything else
        var prog = try ast.Program.init(self.allocator);
        errdefer prog.deinit();

        while (self.cur_tok != .eof) {
            const stmt = try self.parseStatement();
            // program cleans up statements in case of error
            try prog.statements.append(stmt);
            self.nextToken();
        }

        return prog;
    }

    fn parseStatement(self: *Parser) !*ast.Statement {
        switch (self.cur_tok) {
            .let => return self.parseLetStatement(),
            else => return error.UnhandeledToken,
        }
    }

    fn parseLetStatement(self: *Parser) !*ast.Statement {
        // statements handled by program, not the parser
        // maybe it was not the smartest choice
        const stmt = try self.allocator.create(ast.Statement);
        errdefer self.allocator.destroy(stmt);

        const tok = self.cur_tok;

        std.debug.assert(tok == .let);

        if (!self.expectPeek(.ident)) {
            return error.UnexpectedToken;
        }

        const ident = try self.allocator_arena.create(ast.Identifier);
        errdefer self.allocator_arena.destroy(ident);
        ident.* = ast.Identifier{
            .token = self.cur_tok,
            .value = self.cur_tok.literal(),
        };

        if (!self.expectPeek(.assign)) {
            return error.UnexpectedToken;
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
};

test "let statements" {
    std.debug.print("\n", .{});

    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    const lex = try lexer.Lexer.init(t.allocator, input);
    defer lex.deinit();

    const par = try Parser.init(t.allocator, lex);
    defer par.deinit();

    const prog = try par.parseProgram();
    defer prog.deinit();

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
