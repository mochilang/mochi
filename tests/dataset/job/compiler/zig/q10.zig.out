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

fn test_Q10_finds_uncredited_voice_actor_in_Russian_movie() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(uncredited_voiced_character, "Ivan") catch unreachable; m.put(russian_movie, "Vodka Dreams") catch unreachable; break :blk m; }}));
}

pub fn main() void {
    const char_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(name, "Ivan") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(name, "Alex") catch unreachable; break :blk m; }};
    const cast_info: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(person_role_id, @as(i32,@intCast(1))) catch unreachable; m.put(role_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "Soldier (voice) (uncredited)") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(11))) catch unreachable; m.put(person_role_id, @as(i32,@intCast(2))) catch unreachable; m.put(role_id, @as(i32,@intCast(1))) catch unreachable; m.put(note, "(voice)") catch unreachable; break :blk m; }};
    const company_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(country_code, "[ru]") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(country_code, "[us]") catch unreachable; break :blk m; }};
    const company_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; break :blk m; }};
    const movie_companies: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(company_id, @as(i32,@intCast(1))) catch unreachable; m.put(company_type_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(11))) catch unreachable; m.put(company_id, @as(i32,@intCast(2))) catch unreachable; m.put(company_type_id, @as(i32,@intCast(1))) catch unreachable; break :blk m; }};
    const role_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(role, "actor") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(role, "director") catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(10))) catch unreachable; m.put(title, "Vodka Dreams") catch unreachable; m.put(production_year, @as(i32,@intCast(2006))) catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(11))) catch unreachable; m.put(title, "Other Film") catch unreachable; m.put(production_year, @as(i32,@intCast(2004))) catch unreachable; break :blk m; }};
    const matches: []const std.AutoHashMap([]const u8, i32) = blk: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (char_name) |chn| { for (cast_info) |ci| { if !((chn.id == ci.person_role_id)) continue; for (role_type) |rt| { if !((rt.id == ci.role_id)) continue; for (title) |t| { if !((t.id == ci.movie_id)) continue; for (movie_companies) |mc| { if !((mc.movie_id == t.id)) continue; for (company_name) |cn| { if !((cn.id == mc.company_id)) continue; for (company_type) |ct| { if !((ct.id == mc.company_type_id)) continue; if !(((std.mem.eql(u8, (std.mem.eql(u8, ((ci.note.contains("(voice)") and ci.note.contains("(uncredited)")) and cn.country_code), "[ru]") and rt.role), "actor") and t.production_year) > @as(i32,@intCast(2005)))) continue; _tmp0.append(blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(character, chn.name) catch unreachable; m.put(movie, t.title) catch unreachable; break :blk m; }) catch unreachable; } } } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(uncredited_voiced_character, _min_int(blk: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (matches) |x| { _tmp2.append(x.character) catch unreachable; } var _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk _tmp3; })) catch unreachable; m.put(russian_movie, _min_int(blk: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (matches) |x| { _tmp4.append(x.movie) catch unreachable; } var _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk _tmp5; })) catch unreachable; break :blk m; }};
    _json(result);
    test_Q10_finds_uncredited_voice_actor_in_Russian_movie();
}
