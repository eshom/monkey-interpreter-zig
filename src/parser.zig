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
    NoPrefixParseFunction,
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

const PrefixParseFn = *const fn (*Parser, Allocator) anyerror!ast.Expression;
const InfixParseFn = *const fn (*Parser, Allocator, ast.Expression) anyerror!ast.Expression;

const Parser = struct {
    lex: *lexer.Lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    errors: ArrayListStrErr,

    parse_fn_buff: [1024]u8,
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

        // Prefix parse functions
        try parser.registerPrefix(token.TokenType.ident, Parser.parseIdentifier);
        try parser.registerPrefix(token.TokenType.int, Parser.parseIntegerLiteral);
        try parser.registerPrefix(token.TokenType.bang, Parser.parsePrefixExpression);
        try parser.registerPrefix(token.TokenType.minus, Parser.parsePrefixExpression);

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
                ParserError.NoPrefixParseFunction, ParserError.UnexpectedToken, ParserError.InvalidStatementToken => |e| {
                    try self.errors.append(try self.formatError(allocator, e));
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
        switch (err) {
            ParserError.InvalidStatementToken => return try std.fmt.allocPrint(allocator, "`{!}`. Unexpected token in statement: `{s}`", .{
                err, self.cur_token.tokenType(),
            }),
            ParserError.UnexpectedToken => return try std.fmt.allocPrint(allocator, "`{!}`. Unexpected token: `{s}` literal: `{s}`", .{
                err, self.cur_token.tokenType(), try self.cur_token.literal(allocator),
            }),
            ParserError.NoPrefixParseFunction => return try std.fmt.allocPrint(allocator, "`{!}`. No prefix parse function for `{s}` found", .{
                err, try self.cur_token.literal(allocator),
            }),
        }
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
            left_expr = try func(self, allocator);
        } else {
            return ParserError.NoPrefixParseFunction;
        }

        return left_expr;
    }

    fn parseIdentifier(self: *Parser, allocator: Allocator) !ast.Expression {
        const out: ast.Expression = .{ .ident = .{
            .token = self.cur_token,
            .value = try self.cur_token.literal(allocator),
        } };

        return out;
    }

    fn parseIntegerLiteral(self: *Parser, allocator: Allocator) !ast.Expression {
        _ = allocator;
        const lit: ast.Expression = .{ .int = .{ .token = self.cur_token, .value = self.cur_token.int } };
        return lit;
    }

    fn parsePrefixExpression(self: *Parser, allocator: Allocator) !ast.Expression {
        var exp: ast.Expression = .{ .prefix = .{
            .token = self.cur_token,
            .op = try self.cur_token.literal(allocator),
            .right = null,
        } };

        self.nextToken();

        const right_exp = try self.parseExpression(Precedence.prefix, allocator);

        exp.prefix.right = try allocator.create(ast.Expression);
        exp.prefix.right.?.* = right_exp;

        return exp;
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

    fn checkErrors(self: *Parser) bool {
        return self.errors.items.len > 0;
    }

    fn printErrors(self: *Parser) void {
        std.log.err("Caught parser errors:", .{});
        for (self.errors.items) |msg| {
            std.log.err("{s}\n", .{msg});
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

    var expected_identifiers: [3]ast.Identifier = .{
        .{ .token = .{ .ident = "x" }, .value = "x" },
        .{ .token = .{ .ident = "y" }, .value = "y" },
        .{ .token = .{ .ident = "foobar" }, .value = "foobar" },
    };

    const expected_statements: [3]ast.Statement = .{
        .{ .let = .{ .token = .{ .let = "let" }, .name = &expected_identifiers[0], .value = null } },
        .{ .let = .{ .token = .{ .let = "let" }, .name = &expected_identifiers[1], .value = null } },
        .{ .let = .{ .token = .{ .let = "let" }, .name = &expected_identifiers[2], .value = null } },
    };

    try t.expectEqualDeep(&expected_statements, prog.statements.items);

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

    const expected_statements: [4]ast.Statement = .{
        .{ .@"return" = .{ .token = .{ .@"return" = "return" }, .return_value = null } },
        .{ .@"return" = .{ .token = .{ .@"return" = "return" }, .return_value = null } },
        .{ .@"return" = .{ .token = .{ .@"return" = "return" }, .return_value = null } },
        .{ .@"return" = .{ .token = .{ .@"return" = "return" }, .return_value = null } },
    };

    try t.expectEqualDeep(&expected_statements, prog.statements.items);

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

    const expected_statements: [1]ast.Statement = .{
        .{ .expression = .{ .token = .{ .ident = "foobar" }, .expression = .{ .ident = .{ .token = .{ .ident = "foobar" }, .value = "foobar" } } } },
    };

    try t.expectEqualDeep(&expected_statements, prog.statements.items);
}

test "integer literal" {
    std.debug.print("\n", .{});

    const input =
        \\5;
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(1, prog.statements.items.len);

    const expected_statements: [1]ast.Statement = .{
        .{ .expression = .{ .token = .{ .int = 5 }, .expression = .{ .int = .{ .token = .{ .int = 5 }, .value = 5 } } } },
    };

    try t.expectEqualDeep(&expected_statements, prog.statements.items);
}

test "prefix operator" {
    std.debug.print("\n", .{});

    const input =
        \\!5;
        \\-15;
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(2, prog.statements.items.len);

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());

    var expected_right: [2]ast.Expression = .{
        .{ .int = .{ .token = .{ .int = 5 }, .value = 5 } },
        .{ .int = .{ .token = .{ .int = 15 }, .value = 15 } },
    };

    const expected_statements: [2]ast.Statement = .{
        .{ .expression = .{ .token = .{ .bang = "!" }, .expression = .{ .prefix = .{ .token = .{ .bang = "!" }, .op = "!", .right = &expected_right[0] } } } },
        .{ .expression = .{ .token = .{ .minus = "-" }, .expression = .{ .prefix = .{ .token = .{ .bang = "-" }, .op = "-", .right = &expected_right[1] } } } },
    };

    // std.debug.print("expectedStatment1:\n {s}\n", .{try expectedStatements[0].string(allocator)});
    // std.debug.print("foundStatement1:\n {s}\n", .{try prog.statements.items[0].string(allocator)});
    //
    // std.debug.print("expectedStatment2:\n {s}\n", .{try expectedStatements[1].string(allocator)});
    // std.debug.print("foundStatement2:\n {s}\n", .{try prog.statements.items[1].string(allocator)});

    for (expected_statements, 0..) |exp, i| {
        try t.expectEqualStrings(try exp.string(allocator), try prog.statements.items[i].string(allocator));
    }
}
