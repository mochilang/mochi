const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn _json(v: anytype) void {
    var buf = std.ArrayList(u8).init(std.heap.page_allocator);
    defer buf.deinit();
    std.json.stringify(v, .{}, buf.writer()) catch unreachable;
    std.debug.print("{s}\n", .{buf.items});
}

fn test_Q6_finds_marvel_movie_with_Robert_Downey() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(movie_keyword, "marvel-cinematic-universe") catch unreachable; m.put(actor_name, "Downey Robert Jr.") catch unreachable; m.put(marvel_movie, "Iron Man 3") catch unreachable; break :blk m; }}));
}

pub fn main() void {
    const cast_info: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(1))) catch unreachable; m.put(person_id, @as(i32,@intCast(101))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(2))) catch unreachable; m.put(person_id, @as(i32,@intCast(102))) catch unreachable; break :blk m; }};
    const keyword: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(100))) catch unreachable; m.put(keyword, "marvel-cinematic-universe") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(200))) catch unreachable; m.put(keyword, "other") catch unreachable; break :blk m; }};
    const movie_keyword: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(1))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(100))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(2))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(200))) catch unreachable; break :blk m; }};
    const name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(101))) catch unreachable; m.put(name, "Downey Robert Jr.") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(102))) catch unreachable; m.put(name, "Chris Evans") catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(title, "Iron Man 3") catch unreachable; m.put(production_year, @as(i32,@intCast(2013))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(title, "Old Movie") catch unreachable; m.put(production_year, @as(i32,@intCast(2000))) catch unreachable; break :blk m; }};
    const result: []const std.AutoHashMap([]const u8, i32) = blk: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (cast_info) |ci| { for (movie_keyword) |mk| { if !((ci.movie_id == mk.movie_id)) continue; for (keyword) |k| { if !((mk.keyword_id == k.id)) continue; for (name) |n| { if !((ci.person_id == n.id)) continue; for (title) |t| { if !((ci.movie_id == t.id)) continue; if !(((((std.mem.eql(u8, k.keyword, "marvel-cinematic-universe") and n.name.contains("Downey")) and n.name.contains("Robert")) and t.production_year) > @as(i32,@intCast(2010)))) continue; _tmp0.append(blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_keyword, k.keyword) catch unreachable; m.put(actor_name, n.name) catch unreachable; m.put(marvel_movie, t.title) catch unreachable; break :blk m; }) catch unreachable; } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    _json(result);
    test_Q6_finds_marvel_movie_with_Robert_Downey();
}
