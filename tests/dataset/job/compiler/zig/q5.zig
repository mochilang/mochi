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

fn test_Q5_finds_the_lexicographically_first_qualifying_title() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(typical_european_movie, "A Film") catch unreachable; break :blk m; }}));
}

pub fn main() void {
    const company_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(ct_id, @as(i32,@intCast(1))) catch unreachable; m.put(kind, "production companies") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(ct_id, @as(i32,@intCast(2))) catch unreachable; m.put(kind, "other") catch unreachable; break :blk m; }};
    const info_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(it_id, @as(i32,@intCast(10))) catch unreachable; m.put(info, "languages") catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(t_id, @as(i32,@intCast(100))) catch unreachable; m.put(title, "B Movie") catch unreachable; m.put(production_year, @as(i32,@intCast(2010))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(t_id, @as(i32,@intCast(200))) catch unreachable; m.put(title, "A Film") catch unreachable; m.put(production_year, @as(i32,@intCast(2012))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(t_id, @as(i32,@intCast(300))) catch unreachable; m.put(title, "Old Movie") catch unreachable; m.put(production_year, @as(i32,@intCast(2000))) catch unreachable; break :blk m; }};
    const movie_companies: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(100))) catch unreachable; m.put(company_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "ACME (France) (theatrical)") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(200))) catch unreachable; m.put(company_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "ACME (France) (theatrical)") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(300))) catch unreachable; m.put(company_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "ACME (France) (theatrical)") catch unreachable; break :blk m; }};
    const movie_info: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(100))) catch unreachable; m.put(info, "German") catch unreachable; m.put(info_type_id, @as(i32,@intCast(10))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(200))) catch unreachable; m.put(info, "Swedish") catch unreachable; m.put(info_type_id, @as(i32,@intCast(10))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(300))) catch unreachable; m.put(info, "German") catch unreachable; m.put(info_type_id, @as(i32,@intCast(10))) catch unreachable; break :blk m; }};
    const candidate_titles: []const i32 = blk: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); for (company_type) |ct| { for (movie_companies) |mc| { if !((mc.company_type_id == ct.ct_id)) continue; for (movie_info) |mi| { if !((mi.movie_id == mc.movie_id)) continue; for (info_type) |it| { if !((it.it_id == mi.info_type_id)) continue; for (title) |t| { if !((t.t_id == mc.movie_id)) continue; if !((((mc.note.contains((mc.note.contains((std.mem.eql(u8, ct.kind, "production companies") and "(theatrical)")) and "(France)")) and t.production_year) > @as(i32,@intCast(2005))) and (_contains_list_string(&[_][]const u8{"Sweden", "Norway", "Germany", "Denmark", "Swedish", "Denish", "Norwegian", "German"}, mi.info)))) continue; _tmp0.append(t.title) catch unreachable; } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(typical_european_movie, _min_int(candidate_titles)) catch unreachable; break :blk m; }};
    _json(result);
    test_Q5_finds_the_lexicographically_first_qualifying_title();
}
