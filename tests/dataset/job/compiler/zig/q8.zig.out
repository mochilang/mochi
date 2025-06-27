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

fn test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing() void {
    expect((result == &[_]std.AutoHashMap([]const u8, []const u8){blk: { var m = std.AutoHashMap(i32, []const u8).init(std.heap.page_allocator); m.put(actress_pseudonym, "Y. S.") catch unreachable; m.put(japanese_movie_dubbed, "Dubbed Film") catch unreachable; break :blk m; }}));
}

pub fn main() void {
    const aka_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(1))) catch unreachable; m.put(name, "Y. S.") catch unreachable; break :blk m; }};
    const cast_info: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(person_id, @as(i32,@intCast(1))) catch unreachable; m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(note, "(voice: English version)") catch unreachable; m.put(role_id, @as(i32,@intCast(1000))) catch unreachable; break :blk m; }};
    const company_name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(50))) catch unreachable; m.put(country_code, "[jp]") catch unreachable; break :blk m; }};
    const movie_companies: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(movie_id, @as(i32,@intCast(10))) catch unreachable; m.put(company_id, @as(i32,@intCast(50))) catch unreachable; m.put(note, "Studio (Japan)") catch unreachable; break :blk m; }};
    const name: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1))) catch unreachable; m.put(name, "Yoko Ono") catch unreachable; break :blk m; }, blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(2))) catch unreachable; m.put(name, "Yuichi") catch unreachable; break :blk m; }};
    const role_type: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(1000))) catch unreachable; m.put(role, "actress") catch unreachable; break :blk m; }};
    const title: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(id, @as(i32,@intCast(10))) catch unreachable; m.put(title, "Dubbed Film") catch unreachable; break :blk m; }};
    const eligible: []const std.AutoHashMap([]const u8, i32) = blk: { var _tmp0 = std.ArrayList(std.AutoHashMap([]const u8, i32)).init(std.heap.page_allocator); for (aka_name) |an1| { for (name) |n1| { if !((n1.id == an1.person_id)) continue; for (cast_info) |ci| { if !((ci.person_id == an1.person_id)) continue; for (title) |t| { if !((t.id == ci.movie_id)) continue; for (movie_companies) |mc| { if !((mc.movie_id == ci.movie_id)) continue; for (company_name) |cn| { if !((cn.id == mc.company_id)) continue; for (role_type) |rt| { if !((rt.id == ci.role_id)) continue; if !(std.mem.eql(u8, (((((std.mem.eql(u8, (std.mem.eql(u8, ci.note, "(voice: English version)") and cn.country_code), "[jp]") and mc.note.contains("(Japan)")) and (!mc.note.contains("(USA)"))) and n1.name.contains("Yo")) and (!n1.name.contains("Yu"))) and rt.role), "actress")) continue; _tmp0.append(blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(pseudonym, an1.name) catch unreachable; m.put(movie_title, t.title) catch unreachable; break :blk m; }) catch unreachable; } } } } } } } var _tmp1 = _tmp0.toOwnedSlice() catch unreachable; break :blk _tmp1; };
    const result: []const std.AutoHashMap([]const u8, i32) = &[_]std.AutoHashMap([]const u8, i32){blk: { var m = std.AutoHashMap(i32, i32).init(std.heap.page_allocator); m.put(actress_pseudonym, _min_int(blk: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); for (eligible) |x| { _tmp2.append(x.pseudonym) catch unreachable; } var _tmp3 = _tmp2.toOwnedSlice() catch unreachable; break :blk _tmp3; })) catch unreachable; m.put(japanese_movie_dubbed, _min_int(blk: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); for (eligible) |x| { _tmp4.append(x.movie_title) catch unreachable; } var _tmp5 = _tmp4.toOwnedSlice() catch unreachable; break :blk _tmp5; })) catch unreachable; break :blk m; }};
    std.debug.print("{any}\n", .{result});
    test_Q8_returns_the_pseudonym_and_movie_title_for_Japanese_dubbing();
}
