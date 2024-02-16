const std = @import("std");
const repl = @import("repl.zig");
const os_tag = @import("builtin").os.tag;

pub fn main() !void {
    var _gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = _gpa.allocator();

    var user: []const u8 = undefined;
    switch (os_tag) {
        .linux => {
            user = std.os.getenv("USER") orelse @panic("no `USER` environment variable\n");
        },
        .windows => {
            const env_var = std.unicode.utf8ToUtf16LeStringLiteral("USERNAME");
            const user16 = std.os.getenvW(env_var) orelse @panic("no `USERNAME` environment variable\n");
            const user_m = try std.unicode.utf16leToUtf8Alloc(gpa, user16);
            user = user_m;
        },
        else => @panic("OS not supported\n"),
    }

    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    try stdout.print("Hello {s}! This is the Monkey programming language!\n", .{user});
    try stdout.print("Feel free to type in commands\n", .{});
    try repl.start(gpa, stdin, stdout);
}
