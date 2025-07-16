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

fn test_Q7_finds_movie_features_biography_for_person() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(of_person, "Alan Brown") catch unreachable; m.put(biography_movie, "Feature Film") catch unreachable; break :blk m; }}));
}

pub fn main() void {
    const aka_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(1))) catch unreachable; m.put(name, "Anna Mae") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(2))) catch unreachable; m.put(name, "Chris") catch unreachable; break :blk m; }};
    const cast_info: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(1))) catch unreachable; m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(2))) catch unreachable; m.put(movie_id, @as(i32,@intCast(20))) catch unreachable; break :blk m; }};
    const info_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(info, "mini biography") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(info, "trivia") catch unreachable; break :blk m; }};
    const link_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(link, "features") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(link, "references") catch unreachable; break :blk m; }};
    const movie_link: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(linked_movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(link_type_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(linked_movie_id, @as(i32,@intCast(20))) catch unreachable; m.put(link_type_id, @as(i32,@intCast(2))) catch unreachable; break :blk m; }};
    const name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(name, "Alan Brown") catch unreachable; m.put(name_pcode_cf, "B") catch unreachable; m.put(gender, "m") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(name, "Zoe") catch unreachable; m.put(name_pcode_cf, "Z") catch unreachable; m.put(gender, "f") catch unreachable; break :blk m; }};
    const person_info: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(1))) catch unreachable; m.put(info_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "Volker Boehm") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(2))) catch unreachable; m.put(info_type_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "Other") catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(10))) catch unreachable; m.put(title, "Feature Film") catch unreachable; m.put(production_year, @as(i32,@intCast(1990))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(20))) catch unreachable; m.put(title, "Late Film") catch unreachable; m.put(production_year, @as(i32,@intCast(2000))) catch unreachable; break :blk m; }};
    const rows: []const std.AutoHashMap([]const u8, i32) = blk: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (aka_name) |an| { for (name) |n| { if !((n.id == an.person_id)) continue; for (person_info) |pi| { if !((pi.person_id == an.person_id)) continue; for (info_type) |it| { if !((it.id == pi.info_type_id)) continue; for (cast_info) |ci| { if !((ci.person_id == n.id)) continue; for (title) |t| { if !((t.id == ci.movie_id)) continue; for (movie_link) |ml| { if !((ml.linked_movie_id == t.id)) continue; for (link_type) |lt| { if !((lt.id == ml.link_type_id)) continue; if !((((((((((((((std.mem.eql(u8, ((((((std.mem.eql(u8, (std.mem.eql(u8, (an.name.contains("a") and it.info), "mini biography") and lt.link), "features") and n.name_pcode_cf) >= "A") and n.name_pcode_cf) <= "F") and ((std.mem.eql(u8, n.gender, "m") or ((std.mem.eql(u8, n.gender, "f") and n.name.starts_with("B")))))) and pi.note), "Volker Boehm") and t.production_year) >= @as(i32,@intCast(1980))) and t.production_year) <= @as(i32,@intCast(1995))) and pi.person_id) == an.person_id) and pi.person_id) == ci.person_id) and an.person_id) == ci.person_id) and ci.movie_id) == ml.linked_movie_id))) continue; _tmp0.append(blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_name, n.name) catch unreachable; m.put(movie_title, t.title) catch unreachable; break :blk m; }) catch unreachable; } } } } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(of_person, _min_int(blk: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (rows) |r| { _tmp2.append(r.person_name) catch unreachable; } var _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk _tmp3; })) catch unreachable; m.put(biography_movie, _min_int(blk: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (rows) |r| { _tmp4.append(r.movie_title) catch unreachable; } var _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk _tmp5; })) catch unreachable; break :blk m; }};
    _json(result);
    test_Q7_finds_movie_features_biography_for_person();
}
