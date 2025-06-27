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

fn _contains_list_string(v: []const []const u8, item: []const u8) bool {
    for (v) |it| { if (std.mem.eql(u8, it, item)) return true; }
    return false;
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

fn test_Q3_returns_lexicographically_smallest_sequel_title() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(movie_title, "Alpha") catch unreachable; break :blk m; }}));
}

pub fn main() void {
    const keyword: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(keyword, "amazing sequel") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(keyword, "prequel") catch unreachable; break :blk m; }};
    const movie_info: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(info, "Germany") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(30))) catch unreachable; m.put(info, "Sweden") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(20))) catch unreachable; m.put(info, "France") catch unreachable; break :blk m; }};
    const movie_keyword: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(30))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(20))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(2))) catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(10))) catch unreachable; m.put(title, "Alpha") catch unreachable; m.put(production_year, @as(i32,@intCast(2006))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(30))) catch unreachable; m.put(title, "Beta") catch unreachable; m.put(production_year, @as(i32,@intCast(2008))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(20))) catch unreachable; m.put(title, "Gamma") catch unreachable; m.put(production_year, @as(i32,@intCast(2009))) catch unreachable; break :blk m; }};
    const allowed_infos: []const []const u8 = &[_][]const u8{"Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"};
    const candidate_titles: []const i32 = blk: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (keyword) |k| { for (movie_keyword) |mk| { if !((mk.keyword_id == k.id)) continue; for (movie_info) |mi| { if !((mi.movie_id == mk.movie_id)) continue; for (title) |t| { if !((t.id == mi.movie_id)) continue; if !(((((_contains_list_string(allowed_infos, (k.keyword.contains("sequel") and mi.info)) and t.production_year) > @as(i32,@intCast(2005))) and mk.movie_id) == mi.movie_id)) continue; _tmp0.append(t.title) catch unreachable; } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_title, _min_int(candidate_titles)) catch unreachable; break :blk m; }};
    _json(result);
    test_Q3_returns_lexicographically_smallest_sequel_title();
}
