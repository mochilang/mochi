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

fn test_Q9_selects_minimal_alternative_name__character_and_movie() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(alternative_name, "A. N. G.") catch unreachable; m.put(character_name, "Angel") catch unreachable; m.put(movie, "Famous Film") catch unreachable; break :blk m; }}));
}

pub fn main() void {
    const aka_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(1))) catch unreachable; m.put(name, "A. N. G.") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(2))) catch unreachable; m.put(name, "J. D.") catch unreachable; break :blk m; }};
    const char_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(10))) catch unreachable; m.put(name, "Angel") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(20))) catch unreachable; m.put(name, "Devil") catch unreachable; break :blk m; }};
    const cast_info: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(1))) catch unreachable; m.put(person_role_id, @as(i32,@intCast(10))) catch unreachable; m.put(movie_id, @as(i32,@intCast(100))) catch unreachable; m.put(role_id, @as(i32,@intCast(1000))) catch unreachable; m.put(note, "(voice)") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(2))) catch unreachable; m.put(person_role_id, @as(i32,@intCast(20))) catch unreachable; m.put(movie_id, @as(i32,@intCast(200))) catch unreachable; m.put(role_id, @as(i32,@intCast(1000))) catch unreachable; m.put(note, "(voice)") catch unreachable; break :blk m; }};
    const company_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(100))) catch unreachable; m.put(country_code, "[us]") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(200))) catch unreachable; m.put(country_code, "[gb]") catch unreachable; break :blk m; }};
    const movie_companies: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(100))) catch unreachable; m.put(company_id, @as(i32,@intCast(100))) catch unreachable; m.put(note, "ACME Studios (USA)") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(200))) catch unreachable; m.put(company_id, @as(i32,@intCast(200))) catch unreachable; m.put(note, "Maple Films") catch unreachable; break :blk m; }};
    const name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(name, "Angela Smith") catch unreachable; m.put(gender, "f") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(name, "John Doe") catch unreachable; m.put(gender, "m") catch unreachable; break :blk m; }};
    const role_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1000))) catch unreachable; m.put(role, "actress") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2000))) catch unreachable; m.put(role, "actor") catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(100))) catch unreachable; m.put(title, "Famous Film") catch unreachable; m.put(production_year, @as(i32,@intCast(2010))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(200))) catch unreachable; m.put(title, "Old Movie") catch unreachable; m.put(production_year, @as(i32,@intCast(1999))) catch unreachable; break :blk m; }};
    const matches: []const std.AutoHashMap([]const u8, i32) = blk: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (aka_name) |an| { for (name) |n| { if !((an.person_id == n.id)) continue; for (cast_info) |ci| { if !((ci.person_id == n.id)) continue; for (char_name) |chn| { if !((chn.id == ci.person_role_id)) continue; for (title) |t| { if !((t.id == ci.movie_id)) continue; for (movie_companies) |mc| { if !((mc.movie_id == t.id)) continue; for (company_name) |cn| { if !((cn.id == mc.company_id)) continue; for (role_type) |rt| { if !((rt.id == ci.role_id)) continue; if !(((((std.mem.eql(u8, ((std.mem.eql(u8, ((std.mem.eql(u8, ((_contains_list_string(&[_][]const u8{"(voice)", "(voice: Japanese version)", "(voice) (uncredited)", "(voice: English version)"}, ci.note)) and cn.country_code), "[us]") and ((mc.note.contains("(USA)") or mc.note.contains("(worldwide)")))) and n.gender), "f") and n.name.contains("Ang")) and rt.role), "actress") and t.production_year) >= @as(i32,@intCast(2005))) and t.production_year) <= @as(i32,@intCast(2015)))) continue; _tmp0.append(blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(alt, an.name) catch unreachable; m.put(character, chn.name) catch unreachable; m.put(movie, t.title) catch unreachable; break :blk m; }) catch unreachable; } } } } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(alternative_name, _min_int(blk: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (matches) |x| { _tmp2.append(x.alt) catch unreachable; } var _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk _tmp3; })) catch unreachable; m.put(character_name, _min_int(blk: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (matches) |x| { _tmp4.append(x.character) catch unreachable; } var _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk _tmp5; })) catch unreachable; m.put(movie, _min_int(blk: { var _tmp6 = std.ArrayList(i32).init(std.heap.page_allocator); for (matches) |x| { _tmp6.append(x.movie) catch unreachable; } var _tmp7 = _tmp6.toOwnedSlice() catch unreachable; break :blk _tmp7; })) catch unreachable; break :blk m; }};
    _json(result);
    test_Q9_selects_minimal_alternative_name__character_and_movie();
}
