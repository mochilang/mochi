const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch |err| handleError(err);
    std.debug.print("{s}\n", .{buf.items});
}

const SupplierItem = struct {
    s_suppkey: i32,
    s_name: []const u8,
    s_address: []const u8,
    s_comment: []const u8,
};
const supplier = &[_]SupplierItem{
    SupplierItem{
    .s_suppkey = 100,
    .s_name = "AlphaSupply",
    .s_address = "123 Hilltop",
    .s_comment = "Reliable and efficient",
},
    SupplierItem{
    .s_suppkey = 200,
    .s_name = "BetaSupply",
    .s_address = "456 Riverside",
    .s_comment = "Known for Customer Complaints",
},
}; // []const SupplierItem
const PartItem = struct {
    p_partkey: i32,
    p_brand: []const u8,
    p_type: []const u8,
    p_size: i32,
};
const part = &[_]PartItem{
    PartItem{
    .p_partkey = 1,
    .p_brand = "Brand#12",
    .p_type = "SMALL ANODIZED",
    .p_size = 5,
},
    PartItem{
    .p_partkey = 2,
    .p_brand = "Brand#23",
    .p_type = "MEDIUM POLISHED",
    .p_size = 10,
},
}; // []const PartItem
const PartsuppItem = struct {
    ps_partkey: i32,
    ps_suppkey: i32,
};
const partsupp = &[_]PartsuppItem{
    PartsuppItem{
    .ps_partkey = 1,
    .ps_suppkey = 100,
},
    PartsuppItem{
    .ps_partkey = 2,
    .ps_suppkey = 200,
},
}; // []const PartsuppItem
var excluded_suppliers: []const i32 = undefined; // []const i32
const ResultItem = struct {
    s_name: []const u8,
    s_address: []const u8,
};
var result: []const ResultItem = undefined; // []const ResultItem

fn test_Q16_returns_suppliers_not_linked_to_certain_parts_or_complaints() void {
    expect((result == &[]i32{}));
}

pub fn main() void {
    excluded_suppliers = blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (partsupp) |ps| { for (part) |p| { if (!((p.p_partkey == ps.ps_partkey))) continue; if (!(((std.mem.eql(u8, p.p_brand, "Brand#12") and std.mem.indexOf(u8, p.p_type, "SMALL") != null) and (p.p_size == 5)))) continue; _tmp0.append(ps.ps_suppkey) catch |err| handleError(err); } } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; };
    result = blk1: { var _tmp3 = std.ArrayList(struct { item: ResultItem, key: i32 }).init(std.heap.page_allocator); for (supplier) |s| { if (!(((!(std.mem.indexOfScalar(i32, excluded_suppliers, s.s_suppkey) != null) and (!std.mem.indexOf(u8, s.s_comment, "Customer") != null)) and (!std.mem.indexOf(u8, s.s_comment, "Complaints") != null)))) continue; _tmp3.append(.{ .item = ResultItem{
    .s_name = s.s_name,
    .s_address = s.s_address,
}, .key = s.s_name }) catch |err| handleError(err); } for (0.._tmp3.items.len) |i| { for (i+1.._tmp3.items.len) |j| { if (_tmp3.items[j].key < _tmp3.items[i].key) { const t = _tmp3.items[i]; _tmp3.items[i] = _tmp3.items[j]; _tmp3.items[j] = t; } } } var _tmp4 = std.ArrayList(ResultItem).init(std.heap.page_allocator);for (_tmp3.items) |p| { _tmp4.append(p.item) catch |err| handleError(err); } const _tmp5 = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp5; };
    _json(result);
    test_Q16_returns_suppliers_not_linked_to_certain_parts_or_complaints();
}
