const std = @import("std");

const Rec = struct { a: i32, b: i32 };

pub fn main() !void {
    var list = [_]Rec{
        .{ .a = 1, .b = 2 },
        .{ .a = 1, .b = 1 },
        .{ .a = 0, .b = 5 },
    };

    std.sort.sort(Rec, &list, {}, struct {
        fn lt(ctx: void, x: Rec, y: Rec) bool {
            _ = ctx;
            if (x.a == y.a) return x.b < y.b;
            return x.a < y.a;
        }
    }.lt);

    for (list) |r| {
        std.debug.print("{{a:{d}, b:{d}}}\n", .{ r.a, r.b });
    }
}
