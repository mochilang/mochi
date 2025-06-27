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

fn test_Q2_finds_earliest_title_for_German_companies_with_character_keyword() void {
    expect(std.mem.eql(u8, result, "Der Film"));
}

pub fn main() void {
    const company_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(country_code, "[de]") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(country_code, "[us]") catch unreachable; break :blk m; }};
    const keyword: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(keyword, "character-name-in-title") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(keyword, "other") catch unreachable; break :blk m; }};
    const movie_companies: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(100))) catch unreachable; m.put(company_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(200))) catch unreachable; m.put(company_id, @as(i32,@intCast(2))) catch unreachable; break :blk m; }};
    const movie_keyword: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(100))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(200))) catch unreachable; m.put(keyword_id, @as(i32,@intCast(2))) catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(100))) catch unreachable; m.put(title, "Der Film") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(200))) catch unreachable; m.put(title, "Other Movie") catch unreachable; break :blk m; }};
    const titles: []const i32 = blk: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (company_name) |cn| { for (movie_companies) |mc| { if !((mc.company_id == cn.id)) continue; for (title) |t| { if !((mc.movie_id == t.id)) continue; for (movie_keyword) |mk| { if !((mk.movie_id == t.id)) continue; for (keyword) |k| { if !((mk.keyword_id == k.id)) continue; if !(((std.mem.eql(u8, (std.mem.eql(u8, cn.country_code, "[de]") and k.keyword), "character-name-in-title") and mc.movie_id) == mk.movie_id)) continue; _tmp0.append(t.title) catch unreachable; } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: i32 = _min_int(titles);
    _json(result);
    test_Q2_finds_earliest_title_for_German_companies_with_character_keyword();
}
