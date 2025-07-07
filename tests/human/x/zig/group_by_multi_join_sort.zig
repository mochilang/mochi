const std = @import("std");

const Nation = struct { n_nationkey: i32, n_name: []const u8 };
const Customer = struct {
    c_custkey: i32,
    c_name: []const u8,
    c_acctbal: f64,
    c_nationkey: i32,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
};
const Order = struct { o_orderkey: i32, o_custkey: i32, o_orderdate: []const u8 };
const Line = struct { l_orderkey: i32, l_returnflag: []const u8, l_extendedprice: f64, l_discount: f64 };

const Row = struct {
    c_custkey: i32,
    c_name: []const u8,
    revenue: f64,
    c_acctbal: f64,
    n_name: []const u8,
    c_address: []const u8,
    c_phone: []const u8,
    c_comment: []const u8,
};

pub fn main() !void {
    const nations = [_]Nation{ .{ .n_nationkey = 1, .n_name = "BRAZIL" } };
    const customers = [_]Customer{ .{
        .c_custkey = 1,
        .c_name = "Alice",
        .c_acctbal = 100.0,
        .c_nationkey = 1,
        .c_address = "123 St",
        .c_phone = "123-456",
        .c_comment = "Loyal",
    } };
    const orders = [_]Order{
        .{ .o_orderkey = 1000, .o_custkey = 1, .o_orderdate = "1993-10-15" },
        .{ .o_orderkey = 2000, .o_custkey = 1, .o_orderdate = "1994-01-02" },
    };
    const lines = [_]Line{
        .{ .l_orderkey = 1000, .l_returnflag = "R", .l_extendedprice = 1000.0, .l_discount = 0.1 },
        .{ .l_orderkey = 2000, .l_returnflag = "N", .l_extendedprice = 500.0, .l_discount = 0.0 },
    };

    const start_date = "1993-10-01";
    const end_date = "1994-01-01";

    var groups = std.AutoHashMap(i32, Row).init(std.heap.page_allocator);
    defer groups.deinit();

    for (customers) |c| {
        for (orders) |o| if (o.o_custkey == c.c_custkey) {
            if (!(std.mem.compare(u8, o.o_orderdate, start_date) >= 0 and std.mem.compare(u8, o.o_orderdate, end_date) < 0))
                continue;
            for (lines) |l| if (l.l_orderkey == o.o_orderkey and std.mem.eql(u8, l.l_returnflag, "R")) {
                for (nations) |n| if (n.n_nationkey == c.c_nationkey) {
                    const rev = l.l_extendedprice * (1.0 - l.l_discount);
                    if (groups.getPtr(c.c_custkey)) |r| {
                        r.revenue += rev;
                    } else {
                        try groups.put(c.c_custkey, .{
                            .c_custkey = c.c_custkey,
                            .c_name = c.c_name,
                            .revenue = rev,
                            .c_acctbal = c.c_acctbal,
                            .n_name = n.n_name,
                            .c_address = c.c_address,
                            .c_phone = c.c_phone,
                            .c_comment = c.c_comment,
                        });
                    }
                }
            }
        }
    }

    var arr = std.ArrayList(Row).init(std.heap.page_allocator);
    defer arr.deinit();
    var it = groups.iterator();
    while (it.next()) |kv| {
        try arr.append(kv.value);
    }

    std.sort.sort(Row, arr.items, {}, struct {
        fn lt(ctx: void, a: Row, b: Row) bool { _ = ctx; return a.revenue > b.revenue; }
    }.lt);

    for (arr.items) |r| {
        std.debug.print("{d} {s} revenue {d}\n", .{ r.c_custkey, r.c_name, r.revenue });
    }
}
