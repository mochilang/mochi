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

fn test_Q4_returns_minimum_rating_and_title_for_sequels() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(rating, "6.2") catch unreachable; m.put(movie_title, "Alpha Movie") catch unreachable; break :blk m; }}));
}

pub fn main() void {
    const info_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(info, "rating") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(info, "other") catch unreachable; break :blk m; }};
    const keyword: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(keyword, "great sequel") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(keyword, "prequel") catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(10))) catch unreachable; m.put(title, "Alpha Movie") catch unreachable; m.put(production_year, @as(i32,@intCast(2006))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(20))) catch unreachable; m.put(title, "Beta Film") catch unreachable; m.put(production_year, @as(i32,@intCast(2007))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(30))) catch unreachable; m.put(title, "Old Film") catch unreachable; m.put(production_year, @as(i32,@intCast(2004))) catch unreachable; break :blk m; }};
    const movie_keyword: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(20))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(30))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }};
    const movie_info_idx: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(info_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(info, "6.2") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(20))) catch unreachable; m.put(info_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(info, "7.8") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(30))) catch unreachable; m.put(info_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(info, "4.5") catch unreachable; break :blk m; }};
    const rows: []const std.AutoHashMap([]const u8, i32) = blk: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (info_type) |it| { for (movie_info_idx) |mi| { if !((it.id == mi.info_type_id)) continue; for (title) |t| { if !((t.id == mi.movie_id)) continue; for (movie_keyword) |mk| { if !((mk.movie_id == t.id)) continue; for (keyword) |k| { if !((k.id == mk.keyword_id)) continue; if !((((((((std.mem.eql(u8, it.info, "rating") and k.keyword.contains("sequel")) and mi.info) > "5.0") and t.production_year) > @as(i32,@intCast(2005))) and mk.movie_id) == mi.movie_id)) continue; _tmp0.append(blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(rating, mi.info) catch unreachable; m.put(title, t.title) catch unreachable; break :blk m; }) catch unreachable; } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(rating, _min_int(blk: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (rows) |r| { _tmp2.append(r.rating) catch unreachable; } var _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk _tmp3; })) catch unreachable; m.put(movie_title, _min_int(blk: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (rows) |r| { _tmp4.append(r.title) catch unreachable; } var _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk _tmp5; })) catch unreachable; break :blk m; }};
    _json(result);
    test_Q4_returns_minimum_rating_and_title_for_sequels();
}
