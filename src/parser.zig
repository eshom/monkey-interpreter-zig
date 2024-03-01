const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const Allocator = std.mem.Allocator;
const t = std.testing;
const ArrayListStrErr = std.ArrayList([]const u8);
const ArrayListIdent = std.ArrayList(ast.Identifier);
const ArrayListExpr = std.ArrayList(ast.Expression);

const ParserError = error{
    InvalidStatementToken,
    UnexpectedToken,
    NoPrefixParseFunction,
    UnmatchedParenthesis,
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
const InfixParseFn = *const fn (*Parser, Allocator, *ast.Expression) anyerror!ast.Expression;

const Parser = struct {
    lex: *lexer.Lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    errors: ArrayListStrErr,
    trace_calls: struct { bool, usize } = .{ false, 0 }, // set to true to enable debug tracing

    parse_fn_buff: [1024 * 2]u8,
    parse_fn_allocator: Allocator,

    prefix_parse_fns: std.AutoHashMap(token.TokenType, PrefixParseFn),
    infix_parse_fns: std.AutoHashMap(token.TokenType, InfixParseFn),

    precedence_table: std.AutoHashMap(token.TokenType, Precedence),

    fn trace(self: *Parser, msg: []const u8) void {
        if (self.trace_calls.@"0") {
            for (0..self.trace_calls.@"1") |_| {
                std.debug.print(" " ** 8, .{});
            }
            std.debug.print("BEGIN {s}\n", .{msg});
            for (0..self.trace_calls.@"1") |_| {
                std.debug.print(" " ** 8, .{});
            }
            std.debug.print("BEGIN token: {s}\n", .{self.cur_token.tokenType()});
            self.trace_calls.@"1" += 1;
        }
    }

    fn untrace(self: *Parser, msg: []const u8) void {
        if (self.trace_calls.@"0") {
            self.trace_calls.@"1" -= 1;
            for (0..self.trace_calls.@"1") |_| {
                std.debug.print(" " ** 8, .{});
            }
            std.debug.print("END token: {s}\n", .{self.cur_token.tokenType()});
            for (0..self.trace_calls.@"1") |_| {
                std.debug.print(" " ** 8, .{});
            }
            std.debug.print("END {s}\n", .{msg});
        }
    }

    pub fn new(allocator: Allocator, lex: *lexer.Lexer) !*Parser {
        const parser = try allocator.create(Parser);
        parser.lex = lex;
        parser.peek_token = token.Token{ .illegal = "" };
        parser.errors = ArrayListStrErr.init(allocator);

        var fixed_buff = std.heap.FixedBufferAllocator.init(&parser.parse_fn_buff);
        parser.parse_fn_allocator = fixed_buff.allocator();

        parser.prefix_parse_fns = std.AutoHashMap(token.TokenType, PrefixParseFn).init(parser.parse_fn_allocator);
        parser.infix_parse_fns = std.AutoHashMap(token.TokenType, InfixParseFn).init(parser.parse_fn_allocator);
        parser.precedence_table = std.AutoHashMap(token.TokenType, Precedence).init(parser.parse_fn_allocator);

        parser.nextToken();
        parser.nextToken();

        // Populate precedence table
        const tokens = comptime [_]token.TokenType{ .eq, .not_eq, .lt, .gt, .plus, .minus, .slash, .asterix, .lparen };
        const precedences = comptime [_]Precedence{ .equals, .equals, .lessgreater, .lessgreater, .sum, .sum, .product, .product, .call };

        inline for (tokens, precedences) |tok_t, prec| {
            try parser.precedence_table.put(tok_t, prec);
        }

        // Prefix parse functions
        try parser.registerPrefix(token.TokenType.ident, Parser.parseIdentifier);
        try parser.registerPrefix(token.TokenType.int, Parser.parseIntegerLiteral);
        try parser.registerPrefix(token.TokenType.true, Parser.parseBoolean);
        try parser.registerPrefix(token.TokenType.false, Parser.parseBoolean);
        try parser.registerPrefix(token.TokenType.bang, Parser.parsePrefixExpression);
        try parser.registerPrefix(token.TokenType.minus, Parser.parsePrefixExpression);
        try parser.registerPrefix(token.TokenType.lparen, Parser.parseGroupedExpression);
        try parser.registerPrefix(token.TokenType.@"if", Parser.parseIfExpression);
        try parser.registerPrefix(token.TokenType.function, Parser.parseFunctionLiteral);

        // Infix parse functions
        try parser.registerInfix(token.TokenType.plus, Parser.parseInfixExpression);
        try parser.registerInfix(token.TokenType.minus, Parser.parseInfixExpression);
        try parser.registerInfix(token.TokenType.slash, Parser.parseInfixExpression);
        try parser.registerInfix(token.TokenType.asterix, Parser.parseInfixExpression);
        try parser.registerInfix(token.TokenType.eq, Parser.parseInfixExpression);
        try parser.registerInfix(token.TokenType.not_eq, Parser.parseInfixExpression);
        try parser.registerInfix(token.TokenType.lt, Parser.parseInfixExpression);
        try parser.registerInfix(token.TokenType.gt, Parser.parseInfixExpression);
        try parser.registerInfix(token.TokenType.lparen, Parser.parseCallExpression);

        return parser;
    }

    fn peekPrecedence(self: *Parser) Precedence {
        const prec = self.precedence_table.get(std.meta.activeTag(self.peek_token));
        if (prec) |p| {
            return p;
        } else {
            return Precedence.lowest;
        }
    }

    fn curPrecedence(self: *Parser) Precedence {
        const prec = self.precedence_table.get(std.meta.activeTag(self.cur_token));
        if (prec) |p| {
            return p;
        } else {
            return Precedence.lowest;
        }
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
        self.trace("parseProgram");
        defer self.untrace("parseProgram");

        const prog = try ast.Program.new(allocator);

        while (self.cur_token != token.Token.eof) {
            const node = self.parseStatement(allocator) catch |err| switch (err) {
                ParserError.NoPrefixParseFunction, ParserError.UnexpectedToken, ParserError.InvalidStatementToken, ParserError.UnmatchedParenthesis => |e| {
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
            ParserError.UnmatchedParenthesis => return try std.fmt.allocPrint(allocator, "`{!}`. Unmatched parenthesis while parsing `{s}`", .{
                err, try self.cur_token.literal(allocator),
            }),
        }
    }

    fn parseStatement(self: *Parser, allocator: Allocator) !*ast.Node {
        self.trace("parseStatement");
        defer self.untrace("parseStatement");

        switch (self.cur_token) {
            .let => return try self.parseLetStatement(allocator),
            .@"return" => return try self.parseReturnStatement(allocator),
            else => return self.parseExpressionStatement(allocator),
        }
    }

    fn parseExpressionStatement(self: *Parser, allocator: Allocator) !*ast.Node {
        self.trace("parseExpressionStatement");
        defer self.untrace("parseExpressionStatement");

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
        self.trace("parseExpression");
        defer self.untrace("parseExpression");

        const prefix = self.prefix_parse_fns.get(std.meta.activeTag(self.cur_token));
        var left_expr: ast.Expression = undefined;

        if (prefix) |func| {
            left_expr = try func(self, allocator);
        } else {
            return ParserError.NoPrefixParseFunction;
        }

        var left_expr_prefix = left_expr;
        var expr_infix = left_expr;

        while (self.peek_token != .semicolon and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
            const infix = self.infix_parse_fns.get(std.meta.activeTag(self.peek_token));
            if (infix) |func| {
                self.nextToken();
                // if (std.meta.activeTag(left_expr) == .ident) std.debug.print("left_expr: {s}\n", .{left_expr.ident.value});
                expr_infix = try func(self, allocator, &left_expr_prefix);
            } else {
                return left_expr_prefix;
            }
        }

        if (std.meta.activeTag(expr_infix) == .call) {
            std.debug.print("Call function: {any}\n", .{expr_infix.call.function});
        }
        return expr_infix;
    }

    fn parseGroupedExpression(self: *Parser, allocator: Allocator) !ast.Expression {
        self.trace("parseGroupedExpression");
        defer self.untrace("parseGroupedExpression");

        self.nextToken();

        const exp = try self.parseExpression(Precedence.lowest, allocator);

        if (!self.expectPeek(.rparen)) {
            return ParserError.UnmatchedParenthesis;
        }

        return exp;
    }

    fn parseIdentifier(self: *Parser, allocator: Allocator) !ast.Expression {
        self.trace("parseIdentifier");
        defer self.untrace("parseIdentifier");

        const out: ast.Expression = .{ .ident = .{
            .token = self.cur_token,
            .value = try self.cur_token.literal(allocator),
        } };

        return out;
    }

    fn parseIntegerLiteral(self: *Parser, allocator: Allocator) !ast.Expression {
        self.trace("parseIntegerLiteral");
        defer self.untrace("parseIntegerLiteral");

        std.debug.assert(self.cur_token == .int);
        _ = allocator;
        const lit: ast.Expression = .{ .int = .{ .token = self.cur_token, .value = self.cur_token.int } };
        return lit;
    }

    fn parsePrefixExpression(self: *Parser, allocator: Allocator) !ast.Expression {
        self.trace("parsePrefixExpression");
        defer self.untrace("parsePrefixExpression");

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

    fn parseInfixExpression(self: *Parser, allocator: Allocator, left: *ast.Expression) !ast.Expression {
        self.trace("parseInfixExpression");
        defer self.untrace("parseInfixExpression");

        var exp: ast.Expression = .{ .infix = .{
            .token = self.cur_token,
            .op = try self.cur_token.literal(allocator),
            .left = null,
            .right = null,
        } };

        const precedence = self.curPrecedence();
        self.nextToken();

        const right_exp = try self.parseExpression(precedence, allocator);

        exp.infix.right = try allocator.create(ast.Expression);
        exp.infix.right.?.* = right_exp;

        exp.infix.left = try allocator.create(ast.Expression);
        exp.infix.left.?.* = left.*;

        return exp;
    }

    fn parseBoolean(self: *Parser, allocator: Allocator) !ast.Expression {
        self.trace("parseBoolean");
        defer self.untrace("parseBoolean");

        _ = allocator;
        return .{ .boolean = .{ .token = self.cur_token, .value = self.curTokenIs(token.TokenType.true) } };
    }

    fn parseReturnStatement(self: *Parser, allocator: Allocator) !*ast.Node {
        self.trace("parseReturnStatement");
        defer self.untrace("parseReturnStatement");

        const rtn = try allocator.create(ast.Node);
        rtn.* = .{ .statement = .{ .@"return" = .{
            .token = self.cur_token,
            .return_value = null,
        } } };

        self.nextToken();

        rtn.statement.@"return".return_value = self.parseExpression(Precedence.lowest, allocator) catch |err| blk: {
            // return statements can be independent of expressions
            if (!self.curTokenIs(.semicolon)) {
                return err;
            } else {
                break :blk null;
            }
        };

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        return rtn;
    }

    // program needs to cleanup a bunch of types allocated here
    fn parseLetStatement(self: *Parser, allocator: Allocator) !*ast.Node {
        self.trace("parseLetStatement");
        defer self.untrace("parseLetStatement");

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

        self.nextToken();

        let.statement.let.value = try self.parseExpression(Precedence.lowest, allocator);

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        return let;
    }

    fn parseIfExpression(self: *Parser, allocator: Allocator) !ast.Expression {
        self.trace("parseIfExpression");
        defer self.untrace("parseIfExpression");

        std.debug.assert(self.curTokenIs(.@"if"));
        const tok_if = self.cur_token;

        const cond = try allocator.create(ast.Expression);
        errdefer allocator.destroy(cond);

        if (!self.expectPeek(.lparen)) {
            return ParserError.UnexpectedToken;
        }

        self.nextToken();

        cond.* = try self.parseExpression(Precedence.lowest, allocator);

        if (!self.expectPeek(.rparen)) {
            return ParserError.UnexpectedToken;
        }

        if (!self.expectPeek(.lbrace)) {
            return ParserError.UnexpectedToken;
        }

        std.debug.assert(self.curTokenIs(.lbrace));

        const conseq = try self.parseBlockStatement(allocator);
        errdefer allocator.destroy(conseq);
        errdefer conseq.statements.deinit();

        std.debug.assert(self.curTokenIs(.rbrace));

        var exp: ast.Expression = .{ .@"if" = .{
            .token = tok_if,
            .condition = cond,
            .consequence = conseq,
            .alternative = null,
        } };

        if (self.peekTokenIs(.@"else")) {
            self.nextToken();

            if (!self.expectPeek(.lbrace)) {
                return ParserError.UnexpectedToken;
            }

            const alt = try self.parseBlockStatement(allocator);
            errdefer allocator.destroy(alt);
            errdefer alt.statements.deinit();

            std.debug.assert(self.curTokenIs(.rbrace));

            exp.@"if".alternative = alt;
        }

        return exp;
    }

    fn parseBlockStatement(self: *Parser, allocator: Allocator) !*ast.BlockStatement {
        self.trace("parseBlockStatement");
        defer self.untrace("parseBlockStatement");

        const block = try ast.BlockStatement.new(allocator);
        errdefer allocator.destroy(block);
        errdefer block.statements.deinit();

        self.nextToken();

        while (!self.curTokenIs(.rbrace) and !self.curTokenIs(.eof)) {
            const stmt = try self.parseStatement(allocator);
            try block.statements.append(stmt.statement);
            self.nextToken();
        }

        std.debug.assert(self.curTokenIs(.rbrace));

        return block;
    }

    fn parseFunctionLiteral(self: *Parser, allocator: Allocator) !ast.Expression {
        self.trace("parseFunctionLiteral");
        defer self.untrace("parseFunctionLiteral");

        std.debug.assert(self.curTokenIs(.function));
        const tok_fn = self.cur_token;

        if (!self.expectPeek(.lparen)) {
            return ParserError.UnexpectedToken;
        }

        const params = try self.parseFunctionParameters(allocator);
        errdefer params.deinit();

        std.debug.assert(self.curTokenIs(.rparen));

        if (!self.expectPeek(.lbrace)) {
            return ParserError.UnexpectedToken;
        }

        const body = try self.parseBlockStatement(allocator);
        errdefer body.statements.deinit();

        std.debug.assert(self.curTokenIs(.rbrace));

        const exp: ast.Expression = .{ .function = .{
            .token = tok_fn,
            .parameters = params,
            .body = body,
        } };

        return exp;
    }

    fn parseFunctionParameters(self: *Parser, allocator: Allocator) !ArrayListIdent {
        self.trace("parseFunctionParameters");
        defer self.untrace("parseFunctionParameters");

        var params = ArrayListIdent.init(allocator);
        errdefer params.deinit();

        if (self.peekTokenIs(.rparen)) {
            self.nextToken();
            return params;
        }

        self.nextToken();
        std.debug.assert(self.curTokenIs(.ident));

        var ident = ast.Identifier{ .token = self.cur_token, .value = try self.cur_token.literal(allocator) };
        try params.append(ident);

        while (self.peekTokenIs(.comma)) {
            self.nextToken();
            std.debug.assert(self.curTokenIs(.comma));
            self.nextToken();
            std.debug.assert(self.curTokenIs(.ident));
            ident = ast.Identifier{ .token = self.cur_token, .value = try self.cur_token.literal(allocator) };
            try params.append(ident);
        }

        if (!self.expectPeek(.rparen)) {
            return ParserError.UnexpectedToken;
        }

        return params;
    }

    fn parseCallExpression(self: *Parser, allocator: Allocator, function: *ast.Expression) !ast.Expression {
        self.trace("parseCallExpression");
        defer self.untrace("parseCallExpression");

        std.debug.assert(self.curTokenIs(.lparen));
        const tok = self.cur_token;

        const args = try self.parseCallArguments(allocator);
        errdefer args.deinit();

        return .{ .call = .{
            .token = tok,
            .function = function,
            .arguments = args,
        } };
    }

    fn parseCallArguments(self: *Parser, allocator: Allocator) !ArrayListExpr {
        var args = ArrayListExpr.init(allocator);
        errdefer args.deinit();

        if (self.peekTokenIs(.rparen)) {
            self.nextToken();
            return args;
        }

        self.nextToken();

        try args.append(try self.parseExpression(Precedence.lowest, allocator));

        while (self.peekTokenIs(.comma)) {
            self.nextToken();
            self.nextToken();
            try args.append(try self.parseExpression(Precedence.lowest, allocator));
        }

        if (!self.expectPeek(.rparen)) {
            return ParserError.UnexpectedToken;
        }

        return args;
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

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());

    const expected_strings: [3][]const u8 = .{
        "let x = 5;",
        "let y = 10;",
        "let foobar = 838383;",
    };

    for (expected_strings, prog.statements.items) |exp, stmt| {
        //std.debug.print("expected: {s}, found: {s}\n", .{ exp, try stmt.string(allocator) });
        try t.expectEqualStrings(exp, try stmt.string(allocator));
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
    //par.trace_calls = .{ true, 0 };
    const prog = try par.parseProgram(allocator);

    //std.debug.print("prog: {s}\n", .{try prog.string(allocator)});

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());

    try t.expectEqual(4, prog.statements.items.len);

    const expected_strings: [4][]const u8 = .{
        "return x;",
        "return ((y + x) + 5);",
        "return ;",
        "return ((x + y) - 5);",
    };

    for (expected_strings, prog.statements.items) |exp, stmt| {
        //std.debug.print("expected: {s}, found: {s}\n", .{ exp, try stmt.string(allocator) });
        try t.expectEqualStrings(exp, try stmt.string(allocator));
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

test "infix parsing" {
    std.debug.print("\n", .{});

    const input =
        \\5 + 5;
        \\5 - 5;
        \\5 * 5;
        \\5 / 5;
        \\5 > 5;
        \\5 < 5;
        \\5 == 5;
        \\5 != 5;
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(8, prog.statements.items.len);

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());

    const expected_strings: [8][]const u8 = .{
        "(5 + 5)",
        "(5 - 5)",
        "(5 * 5)",
        "(5 / 5)",
        "(5 > 5)",
        "(5 < 5)",
        "(5 == 5)",
        "(5 != 5)",
    };

    for (expected_strings, prog.statements.items) |exp, stmt| {
        //std.debug.print("expected: {s}, found: {s}\n", .{ exp, try stmt.string(allocator) });
        try t.expectEqualStrings(exp, try stmt.string(allocator));
    }
}

test "operator precedence parsing" {
    std.debug.print("\n", .{});

    const input =
        \\-a * b
        \\!-a
        \\a + b + c
        \\a + b - c
        \\a * b * c
        \\a * b / c
        \\a + b / c
        \\a + b * c + d / e - f
        \\3 + 4; -5 * 5
        \\5 > 4 == 3 < 4
        \\5 < 4 != 3 > 4
        \\3 + 4 * 5 == 3 * 1 + 4 * 5
        \\3 + 4 * 5 == 3 * 1 + 4 * 5
        \\
        \\(3 + 4) * 5
        \\(3 + (6 * 1)) / 8
        \\1 * (-(5 + 5))
        \\!(true == true)
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    //par.trace_calls = .{ true, 0 };
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(18, prog.statements.items.len);

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());

    const expected_strings: [18][]const u8 = .{
        "((-a) * b)",
        "(!(-a))",
        "((a + b) + c)",
        "((a + b) - c)",
        "((a * b) * c)",
        "((a * b) / c)",
        "(a + (b / c))",
        "(((a + (b * c)) + (d / e)) - f)",
        "(3 + 4)",
        "((-5) * 5)",
        "((5 > 4) == (3 < 4))",
        "((5 < 4) != (3 > 4))",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        "((3 + 4) * 5)",
        "((3 + (6 * 1)) / 8)",
        "(1 * (-(5 + 5)))",
        "(!(true == true))",
    };

    for (expected_strings, prog.statements.items) |exp, stmt| {
        //std.debug.print("expected: {s}, found: {s}\n", .{ exp, try stmt.string(allocator) });
        try t.expectEqualStrings(exp, try stmt.string(allocator));
    }

    // try ast.Program.prettyPrint(try prog.string(allocator), std.io.getStdOut().writer());
}

test "boolean parsing" {
    std.debug.print("\n", .{});

    const input =
        \\true;
        \\false;
        \\let foobar = true;
        \\let barfoo = false;
        \\return true;
        \\return false;
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    //par.trace_calls = .{ true, 0 };
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(6, prog.statements.items.len);

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());

    const expected_strings: [6][]const u8 = .{
        "true",
        "false",
        "let foobar = true;",
        "let barfoo = false;",
        "return true;",
        "return false;",
    };

    for (expected_strings, prog.statements.items) |exp, stmt| {
        //std.debug.print("expected: {s}, found: {s}\n", .{ exp, try stmt.string(allocator) });
        try t.expectEqualStrings(exp, try stmt.string(allocator));
    }
}

test "if expressions" {
    std.debug.print("\n", .{});

    const input =
        \\if (x < y) { x }
        \\if (x < y) { x } else { y }
        \\if (x == y) { x + y; x / y; } else { y; x; }
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    //par.trace_calls = .{ true, 0 };
    const prog = try par.parseProgram(allocator);

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());
    try t.expectEqual(3, prog.statements.items.len);

    const expected_strings: [3][]const u8 = .{
        "if (x < y) { x; }",
        "if (x < y) { x; } else { y; }",
        "if (x == y) { (x + y); (x / y); } else { y; x; }",
    };

    for (expected_strings, prog.statements.items) |exp, stmt| {
        //std.debug.print("expected: {s}, found: {s}\n", .{ exp, try stmt.string(allocator) });
        try t.expectEqualStrings(exp, try stmt.string(allocator));
    }
}

test "function literal" {
    std.debug.print("\n", .{});

    const input =
        \\fn(x, y) { x + y; }
        \\fn() {};
        \\fn(x) {};
        \\fn(x, y, z) {};
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    // par.trace_calls = .{ true, 0 };
    const prog = try par.parseProgram(allocator);

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());
    try t.expectEqual(4, prog.statements.items.len);

    const expected_strings: [4][]const u8 = .{
        "fn(x, y) { (x + y); }",
        "fn() { }",
        "fn(x) { }",
        "fn(x, y, z) { }",
    };

    for (expected_strings, prog.statements.items) |exp, stmt| {
        //std.debug.print("expected: {s}, found: {s}\n", .{ exp, try stmt.string(allocator) });
        try t.expectEqualStrings(exp, try stmt.string(allocator));
    }
}

test "function call" {
    std.debug.print("\n", .{});

    const input =
        \\add(1, 2 * 3, 4 + 5);
    ;

    var prog_arena = std.heap.ArenaAllocator.init(t.allocator);
    defer prog_arena.deinit();
    const allocator = prog_arena.allocator();

    const lex = try lexer.Lexer.new(allocator, input);
    const par = try Parser.new(allocator, lex);
    par.trace_calls = .{ true, 0 };
    const prog = try par.parseProgram(allocator);

    try t.expectEqual(1, prog.statements.items.len);

    if (par.errors.items.len > 0) {
        par.printErrors();
    }

    try t.expect(!par.checkErrors());

    // try ast.Program.prettyPrint(try prog.string(allocator), std.io.getStdOut().writer());

    const expected_strings: [1][]const u8 = .{
        "add((1), (2 * 3), (4 + 5));",
    };

    for (expected_strings, prog.statements.items) |exp, stmt| {
        //std.debug.print("expected: {s}, found: {s}\n", .{ exp, try stmt.string(allocator) });
        try t.expectEqualStrings(exp, try stmt.string(allocator));
    }
}
