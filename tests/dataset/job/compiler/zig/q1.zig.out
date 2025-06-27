const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
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
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

fn test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production() void {
    expect((result == blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(production_note, "ACME (co-production)") catch unreachable; m.put(movie_title, "Good Movie") catch unreachable; m.put(movie_year, @as(i32,@intCast(1995))) catch unreachable; break :blk m; }));
}

pub fn main() void {
    const company_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(kind, "production companies") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(kind, "distributors") catch unreachable; break :blk m; }};
    const info_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(10))) catch unreachable; m.put(info, "top 250 rank") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(20))) catch unreachable; m.put(info, "bottom 10 rank") catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(100))) catch unreachable; m.put(title, "Good Movie") catch unreachable; m.put(production_year, @as(i32,@intCast(1995))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(200))) catch unreachable; m.put(title, "Bad Movie") catch unreachable; m.put(production_year, @as(i32,@intCast(2000))) catch unreachable; break :blk m; }};
    const movie_companies: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(100))) catch unreachable; m.put(company_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "ACME (co-production)") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(200))) catch unreachable; m.put(company_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "MGM (as Metro-Goldwyn-Mayer Pictures)") catch unreachable; break :blk m; }};
    const movie_info_idx: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(100))) catch unreachable; m.put(info_type_id, @as(i32,@intCast(10))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(200))) catch unreachable; m.put(info_type_id, @as(i32,@intCast(20))) catch unreachable; break :blk m; }};
    const filtered: []const std.AutoHashMap([]const u8, i32) = blk: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (company_type) |ct| { for (movie_companies) |mc| { if !((ct.id == mc.company_type_id)) continue; for (title) |t| { if !((t.id == mc.movie_id)) continue; for (movie_info_idx) |mi| { if !((mi.movie_id == t.id)) continue; for (info_type) |it| { if !((it.id == mi.info_type_id)) continue; if !(((std.mem.eql(u8, (std.mem.eql(u8, ct.kind, "production companies") and it.info), "top 250 rank") and (!mc.note.contains("(as Metro-Goldwyn-Mayer Pictures)"))) and ((mc.note.contains("(co-production)") or mc.note.contains("(presents)"))))) continue; _tmp0.append(blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(note, mc.note) catch unreachable; m.put(title, t.title) catch unreachable; m.put(year, t.production_year) catch unreachable; break :blk m; }) catch unreachable; } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: std.AutoHashMap([]const u8, i32) = blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(production_note, _min_int(blk: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |r| { _tmp2.append(r.note) catch unreachable; } var _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk _tmp3; })) catch unreachable; m.put(movie_title, _min_int(blk: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |r| { _tmp4.append(r.title) catch unreachable; } var _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk _tmp5; })) catch unreachable; m.put(movie_year, _min_int(blk: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (filtered) |r| { _tmp6.append(r.year) catch unreachable; } var _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk _tmp7; })) catch unreachable; break :blk m; };
    _json(&[_]std.AutoHashMap([]const u8, i32){result});
    test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production();
}
