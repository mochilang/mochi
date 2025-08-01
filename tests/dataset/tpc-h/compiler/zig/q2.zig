const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _min_int(v: []const i32) i32 {
    if (v.len == 0) return 0;
    var m: i32 = v[0];
    for (v[1..]) |it| { if (it < m) m = it; }
    return m;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch |err| handleError(err);
    std.debug.print("{s}\n", .{buf.items});
}

const RegionItem = struct {
    r_regionkey: i32,
    r_name: []const u8,
};
const region = &[_]RegionItem{
    RegionItem{
    .r_regionkey = 1,
    .r_name = "EUROPE",
},
    RegionItem{
    .r_regionkey = 2,
    .r_name = "ASIA",
},
}; // []const RegionItem
const NationItem = struct {
    n_nationkey: i32,
    n_regionkey: i32,
    n_name: []const u8,
};
const nation = &[_]NationItem{
    NationItem{
    .n_nationkey = 10,
    .n_regionkey = 1,
    .n_name = "FRANCE",
},
    NationItem{
    .n_nationkey = 20,
    .n_regionkey = 2,
    .n_name = "CHINA",
},
}; // []const NationItem
const SupplierItem = struct {
    s_suppkey: i32,
    s_name: []const u8,
    s_address: []const u8,
    s_nationkey: i32,
    s_phone: []const u8,
    s_acctbal: f64,
    s_comment: []const u8,
};
const supplier = &[_]SupplierItem{
    SupplierItem{
    .s_suppkey = 100,
    .s_name = "BestSupplier",
    .s_address = "123 Rue",
    .s_nationkey = 10,
    .s_phone = "123",
    .s_acctbal = 1000.0,
    .s_comment = "Fast and reliable",
},
    SupplierItem{
    .s_suppkey = 200,
    .s_name = "AltSupplier",
    .s_address = "456 Way",
    .s_nationkey = 20,
    .s_phone = "456",
    .s_acctbal = 500.0,
    .s_comment = "Slow",
},
}; // []const SupplierItem
const PartItem = struct {
    p_partkey: i32,
    p_type: []const u8,
    p_size: i32,
    p_mfgr: []const u8,
};
const part = &[_]PartItem{
    PartItem{
    .p_partkey = 1000,
    .p_type = "LARGE BRASS",
    .p_size = 15,
    .p_mfgr = "M1",
},
    PartItem{
    .p_partkey = 2000,
    .p_type = "SMALL COPPER",
    .p_size = 15,
    .p_mfgr = "M2",
},
}; // []const PartItem
const PartsuppItem = struct {
    ps_partkey: i32,
    ps_suppkey: i32,
    ps_supplycost: f64,
};
const partsupp = &[_]PartsuppItem{
    PartsuppItem{
    .ps_partkey = 1000,
    .ps_suppkey = 100,
    .ps_supplycost = 10.0,
},
    PartsuppItem{
    .ps_partkey = 1000,
    .ps_suppkey = 200,
    .ps_supplycost = 15.0,
},
}; // []const PartsuppItem
var europe_nations: []const NationItem = undefined; // []const NationItem
const EuropeSuppliersItem = struct {
    s: SupplierItem,
    n: NationItem,
};
var europe_suppliers: []const EuropeSuppliersItem = undefined; // []const EuropeSuppliersItem
var target_parts: []const PartItem = undefined; // []const PartItem
const TargetPartsuppItem = struct {
    s_acctbal: i32,
    s_name: i32,
    n_name: i32,
    p_partkey: i32,
    p_mfgr: []const u8,
    s_address: i32,
    s_phone: i32,
    s_comment: i32,
    ps_supplycost: f64,
};
var target_partsupp: []const TargetPartsuppItem = undefined; // []const TargetPartsuppItem
var costs: []const i32 = undefined; // []const i32
const min_cost = _min_int(costs); // i32
var result: []const TargetPartsuppItem = undefined; // []const TargetPartsuppItem

fn test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part() void {
    expect((result == &[_]TargetPartsuppItem{TargetPartsuppItem{
    .s_acctbal = 1000.0,
    .s_name = "BestSupplier",
    .n_name = "FRANCE",
    .p_partkey = 1000,
    .p_mfgr = "M1",
    .s_address = "123 Rue",
    .s_phone = "123",
    .s_comment = "Fast and reliable",
    .ps_supplycost = 10.0,
}}));
}

pub fn main() void {
    europe_nations = blk0: { var _tmp0 = std.ArrayList(NationItem).init(std.heap.page_allocator); for (region) |r| { for (nation) |n| { if (!((n.n_regionkey == r.r_regionkey))) continue; if (!(std.mem.eql(u8, r.r_name, "EUROPE"))) continue; _tmp0.append(n) catch |err| handleError(err); } } const _tmp1 = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp1; };
    europe_suppliers = blk1: { var _tmp3 = std.ArrayList(EuropeSuppliersItem).init(std.heap.page_allocator); for (supplier) |s| { for (europe_nations) |n| { if (!((s.s_nationkey == n.n_nationkey))) continue; _tmp3.append(EuropeSuppliersItem{
    .s = s,
    .n = n,
}) catch |err| handleError(err); } } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; };
    target_parts = blk2: { var _tmp5 = std.ArrayList(PartItem).init(std.heap.page_allocator); for (part) |p| { if (!(((p.p_size == 15) and std.mem.eql(u8, p.p_type, "LARGE BRASS")))) continue; _tmp5.append(p) catch |err| handleError(err); } const _tmp6 = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp6; };
    target_partsupp = blk3: { var _tmp8 = std.ArrayList(TargetPartsuppItem).init(std.heap.page_allocator); for (partsupp) |ps| { for (target_parts) |p| { if (!((ps.ps_partkey == p.p_partkey))) continue; for (europe_suppliers) |s| { if (!((ps.ps_suppkey == s.s.s_suppkey))) continue; _tmp8.append(TargetPartsuppItem{
    .s_acctbal = s.s.s_acctbal,
    .s_name = s.s.s_name,
    .n_name = s.n.n_name,
    .p_partkey = p.p_partkey,
    .p_mfgr = p.p_mfgr,
    .s_address = s.s.s_address,
    .s_phone = s.s.s_phone,
    .s_comment = s.s.s_comment,
    .ps_supplycost = ps.ps_supplycost,
}) catch |err| handleError(err); } } } const _tmp9 = _tmp8.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp9; };
    costs = blk4: { var _tmp10 = std.ArrayList(i32).init(std.heap.page_allocator); for (target_partsupp) |x| { _tmp10.append(x.ps_supplycost) catch |err| handleError(err); } const _tmp11 = _tmp10.toOwnedSlice() catch |err| handleError(err); break :blk4 _tmp11; };
    result = blk5: { var _tmp12 = std.ArrayList(struct { item: TargetPartsuppItem, key: i32 }).init(std.heap.page_allocator); for (target_partsupp) |x| { if (!((x.ps_supplycost == min_cost))) continue; _tmp12.append(.{ .item = x, .key = -x.s_acctbal }) catch |err| handleError(err); } for (0.._tmp12.items.len) |i| { for (i+1.._tmp12.items.len) |j| { if (_tmp12.items[j].key < _tmp12.items[i].key) { const t = _tmp12.items[i]; _tmp12.items[i] = _tmp12.items[j]; _tmp12.items[j] = t; } } } var _tmp13 = std.ArrayList(TargetPartsuppItem).init(std.heap.page_allocator);for (_tmp12.items) |p| { _tmp13.append(p.item) catch |err| handleError(err); } const _tmp14 = _tmp13.toOwnedSlice() catch |err| handleError(err); break :blk5 _tmp14; };
    _json(result);
    test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part();
}
