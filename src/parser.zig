const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const Allocator = std.mem.Allocator;
const t = std.testing;
const ArrayListStrErr = std.ArrayList([]const u8);

const ParserError = error{
    InvalidStatementToken,
    UnexpectedToken,
    NoParseFunction,
};

const Precedence = enum {
    lowest,
    equals, // ==
    lessgreater, // > or <
    sum, // +
    product, // *
    prefix, // -X or !X
    call, // myFunction(X)
};

const PrefixParseFn = *const fn (*Parser, Allocator) ast.Expression;
const InfixParseFn = *const fn (*Parser, Allocator, ast.Expression) ast.Expression;

const Parser = struct {
    lex: *lexer.Lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    errors: ArrayListStrErr,

    parse_fn_buff: [1024 * 4]u8,
    parse_fn_allocator: Allocator,

    prefix_parse_fns: std.AutoHashMap(token.TokenType, PrefixParseFn),
    infix_parse_fns: std.AutoHashMap(token.TokenType, InfixParseFn),

    pub fn new(allocator: Allocator, lex: *lexer.Lexer) !*Parser {
        const parser = try allocator.create(Parser);
        parser.lex = lex;
        parser.peek_token = token.Token{ .illegal = "" };
        parser.errors = ArrayListStrErr.init(allocator);

        var fixed_buff = std.heap.FixedBufferAllocator.init(&parser.parse_fn_buff);
        parser.parse_fn_allocator = fixed_buff.allocator();

        parser.prefix_parse_fns = std.AutoHashMap(token.TokenType, PrefixParseFn).init(parser.parse_fn_allocator);
        parser.infix_parse_fns = std.AutoHashMap(token.TokenType, InfixParseFn).init(parser.parse_fn_allocator);

        parser.nextToken();
        parser.nextToken();

        try parser.registerPrefix(token.TokenType.ident, Parser.parseIdentifier);

        return parser;
    }

    fn registerPrefix(self: *Parser, token_type: token.TokenType, func: PrefixParseFn) !void {
        try self.prefix_parse_fns.put(token_type, func);
    }

    fn registerInfix(self: *Parser, token_type: token.TokenType, func: InfixParseFn) !void {
        try self.infix_parse_fns.put(token_type, func);
    }

    fn nextToken(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lex.nextToken();
    }

    pub fn parseProgram(self: *Parser, allocator: Allocator) !*ast.Program {
        const prog = try ast.Program.new(allocator);

        while (self.cur_token != token.Token.eof) {
            const node = self.parseStatement(allocator) catch |err| switch (err) {
                ParserError.InvalidStatementToken => {
                    try self.errors.append(try self.formatError(allocator, ParserError.InvalidStatementToken));
                    self.nextToken();
                    continue;
                },
                ParserError.UnexpectedToken => {
                    try self.errors.append(try self.formatError(allocator, ParserError.UnexpectedToken));
                    self.nextToken();
                    continue;
                },
                else => return err,
            };
            const stmt = node.statement;

            //const lit = try stmt.tokenLiteral(allocator);
            //std.debug.print("Appending: {s}\n", .{lit});
            //std.debug.print("Appending: {any}, literal: {s}\n", .{std.meta.activeTag(stmt), lit});
            //allocator.free(lit);

            try prog.statements.append(stmt);
            self.nextToken();
        }

        return prog;
    }

    fn formatError(self: *Parser, allocator: Allocator, err: ParserError) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "caught parser errors `{!}`:\n    tokenType: .{s}\n    literal: {s}", .{ err, self.cur_token.tokenType(), try self.cur_token.literal(allocator) });
    }

    fn parseStatement(self: *Parser, allocator: Allocator) !*ast.Node {
        switch (self.cur_token) {
            .let => return try self.parseLetStatement(allocator),
            .@"return" => return try self.parseReturnStatement(allocator),
            else => return self.parseExpressionStatement(allocator),
        }
    }

    fn parseExpressionStatement(self: *Parser, allocator: Allocator) !*ast.Node {
        const stmt = try allocator.create(ast.Node);
        const expr = try self.parseExpression(Precedence.lowest, allocator);

        stmt.* = .{ .statement = .{ .expression = .{
            .token = self.cur_token,
            .expression = expr,
        } } };

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        return stmt;
    }

    fn parseExpression(self: *Parser, precedence: Precedence, allocator: Allocator) !ast.Expression {
        const prefix = self.prefix_parse_fns.get(std.meta.activeTag(self.cur_token));
        var left_expr: ast.Expression = undefined;

        _ = precedence;
        if (prefix) |func| {
            std.debug.print("TAG???: {any}\n", .{std.meta.activeTag(self.cur_token)});
            std.debug.print("TAG???: {any}\n", .{.ident});

            left_expr = func(self, allocator);
        } else {
            return ParserError.NoParseFunction;
        }

        return left_expr;
    }

    fn parseIdentifier(self: *Parser, allocator: Allocator) ast.Expression {
        const out: ast.Expression = .{ .ident = .{
            .token = self.cur_token,
            .value = self.cur_token.literal(allocator) catch {
                @panic("failed to allocate literal");
            },
        } };

        return out;
    }

    fn parseReturnStatement(self: *Parser, allocator: Allocator) !*ast.Node {
        const rtn = try allocator.create(ast.Node);
        rtn.* = .{ .statement = .{ .@"return" = .{
            .token = self.cur_token,
            .return_value = null,
        } } };

        self.nextToken();

        //TODO: skipping expressions until semicolon for now
        while (!self.curTokenIs(.semicolon)) {
            self.nextToken();
        }

        return rtn;
    }

    // program needs to cleanup a bunch of types allocated here
    fn parseLetStatement(self: *Parser, allocator: Allocator) !*ast.Node {
        const tok_let = self.cur_token;

        if (!self.expectPeek(.ident)) {
            return ParserError.UnexpectedToken;
        }
        const tok_ident = self.cur_token;

        const ident = try allocator.create(ast.Identifier);
        ident.* = .{
            .token = tok_ident,
            .value = try self.cur_token.literal(allocator),
        };

        const let = try allocator.create(ast.Node);
        let.* = .{ .statement = .{ .let = .{
            .token = tok_let,
            .name = ident,
            .value = null,
        } } };

        if (!self.expectPeek(.assign)) {
            return ParserError.UnexpectedToken;
        }

        //TODO: skipping expressions until semicolon
        while (!self.curTokenIs(.semicolon)) {
            self.nextToken();
        }

        return let;
    }

    fn curTokenIs(self: *Parser, tok: token.TokenType) bool {
        return self.cur_token == tok;
    }

    fn peekTokenIs(self: *Parser, tok: token.TokenType) bool {
        return self.peek_token == tok;
    }

    fn expectPeek(self: *Parser, tok: token.TokenType) bool {
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

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(3, prog.statements.items.len);

    // for (prog.statements.items) |st| {
    //     std.debug.print("statement token literal: {s}\n",.{try st.tokenLiteral(allocator)});
    //     std.debug.print("ident token literal {s}\n", .{try st.let.name.token.literal(allocator)});
    // }

    var expectedIdentifiers: [3]ast.Identifier = .{
        .{ .token = .{ .ident = "x" }, .value = "x" },
        .{ .token = .{ .ident = "y" }, .value = "y" },
        .{ .token = .{ .ident = "foobar" }, .value = "foobar" },
    };

    const expectedStatements: [3]ast.Statement = .{
        .{ .let = .{ .token = .{ .let = "let" }, .name = &expectedIdentifiers[0], .value = null } },
        .{ .let = .{ .token = .{ .let = "let" }, .name = &expectedIdentifiers[1], .value = null } },
        .{ .let = .{ .token = .{ .let = "let" }, .name = &expectedIdentifiers[2], .value = null } },
    };

    try t.expectEqualDeep(&expectedStatements, prog.statements.items);

    if (par.errors.items.len > 0) {
        std.debug.print("Caught parser errors:\n", .{});
        for (par.errors.items) |msg| {
            std.debug.print("{s}\n", .{msg});
        }
    }
}

test "return statements" {
    std.debug.print("\n", .{});

    const input =
        \\return x;
        \\return y + x + 5;
        \\return;
        \\return (x + y) - 5;
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(4, prog.statements.items.len);

    // for (prog.statements.items) |st| {
    //     std.debug.print("statement token literal: {s}\n",.{try st.tokenLiteral(allocator)});
    //     std.debug.print("ident token literal {s}\n", .{try st.let.name.token.literal(allocator)});
    // }

    const expectedStatements: [4]ast.Statement = .{
        .{ .@"return" = .{ .token = .{ .@"return" = "return" }, .return_value = null } },
        .{ .@"return" = .{ .token = .{ .@"return" = "return" }, .return_value = null } },
        .{ .@"return" = .{ .token = .{ .@"return" = "return" }, .return_value = null } },
        .{ .@"return" = .{ .token = .{ .@"return" = "return" }, .return_value = null } },
    };

    try t.expectEqualDeep(&expectedStatements, prog.statements.items);

    if (par.errors.items.len > 0) {
        std.debug.print("Caught parser errors:\n", .{});
        for (par.errors.items) |msg| {
            std.debug.print("{s}\n", .{msg});
        }
    }
}

test "identifier expression" {
    std.debug.print("\n", .{});

    const input =
        \\foobar;
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(1, prog.statements.items.len);

    const expectedStatements: [1]ast.Statement = .{
        .{ .expression = .{ .token = .{ .ident = "foobar" }, .expression = .{ .ident = .{ .token = .{ .ident = "foobar" }, .value = "foobar" } } } },
    };

    try t.expectEqualDeep(&expectedStatements, prog.statements.items);
}
