const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Reader = std.fs.File.Reader;
const Writer = std.fs.File.Writer;

const prompt = ">> ";

pub fn start(allocator: Allocator, in: Reader, out: Writer) !void {
    var input = ArrayList(u8).init(allocator);
    defer input.deinit();

    while (true) {
        try out.print("{s}", .{prompt});
        in.streamUntilDelimiter(input.writer(), '\n', comptime 1024 * 1024) catch |err| {
            if (err == error.EndOfStream) return else return err;
        };

        const lex = try lexer.Lexer.init(allocator, input.items);
        defer allocator.destroy(lex);

        while (true) {
            const tok = lex.nextToken();
            if (tok == token.Token.eof) break;
            try tok.print(out);
            //try out.print("{any}\n", .{tok});
        }

        if (input.items.len > comptime 1024 * 1024 / 2) {
            input.clearAndFree();
        } else {
            input.clearRetainingCapacity();
        }
    }
}
