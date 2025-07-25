// Generated by Mochi compiler v0.10.25 on 2025-07-13T12:55:50Z
const std = @import("std");

fn expect(cond: bool) void {
    if (!cond) @panic("expect failed");
}

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
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
    std.json.stringify(v, .{}, buf.writer()) catch |err| handleError(err);
    std.debug.print("{s}\n", .{buf.items});
}

const InfoTypeItem = struct {
    id: i32,
    info: []const u8,
};
const info_type = &[_]InfoTypeItem{
    InfoTypeItem{
    .id = 1,
    .info = "budget",
},
    InfoTypeItem{
    .id = 2,
    .info = "votes",
},
    InfoTypeItem{
    .id = 3,
    .info = "rating",
},
}; // []const InfoTypeItem
const NameItem = struct {
    id: i32,
    name: []const u8,
    gender: []const u8,
};
const name = &[_]NameItem{
    NameItem{
    .id = 1,
    .name = "Big Tim",
    .gender = "m",
},
    NameItem{
    .id = 2,
    .name = "Slim Tim",
    .gender = "m",
},
    NameItem{
    .id = 3,
    .name = "Alice",
    .gender = "f",
},
}; // []const NameItem
const TitleItem = struct {
    id: i32,
    title: []const u8,
};
const title = &[_]TitleItem{
    TitleItem{
    .id = 10,
    .title = "Alpha",
},
    TitleItem{
    .id = 20,
    .title = "Beta",
},
    TitleItem{
    .id = 30,
    .title = "Gamma",
},
}; // []const TitleItem
const CastInfoItem = struct {
    movie_id: i32,
    person_id: i32,
    note: []const u8,
};
const cast_info = &[_]CastInfoItem{
    CastInfoItem{
    .movie_id = 10,
    .person_id = 1,
    .note = "(producer)",
},
    CastInfoItem{
    .movie_id = 20,
    .person_id = 2,
    .note = "(executive producer)",
},
    CastInfoItem{
    .movie_id = 30,
    .person_id = 3,
    .note = "(producer)",
},
}; // []const CastInfoItem
const MovieInfoItem = struct {
    movie_id: i32,
    info_type_id: i32,
    info: i32,
};
const movie_info = &[_]MovieInfoItem{
    MovieInfoItem{
    .movie_id = 10,
    .info_type_id = 1,
    .info = 90,
},
    MovieInfoItem{
    .movie_id = 20,
    .info_type_id = 1,
    .info = 120,
},
    MovieInfoItem{
    .movie_id = 30,
    .info_type_id = 1,
    .info = 110,
},
}; // []const MovieInfoItem
const MovieInfoIdxItem = struct {
    movie_id: i32,
    info_type_id: i32,
    info: i32,
};
const movie_info_idx = &[_]MovieInfoIdxItem{
    MovieInfoIdxItem{
    .movie_id = 10,
    .info_type_id = 2,
    .info = 500,
},
    MovieInfoIdxItem{
    .movie_id = 20,
    .info_type_id = 2,
    .info = 400,
},
    MovieInfoIdxItem{
    .movie_id = 30,
    .info_type_id = 2,
    .info = 800,
},
}; // []const MovieInfoItem
const ResultStruct0 = struct {
    budget: i32,
    votes: i32,
    title: []const u8,
};
var rows: []const RowsItem = undefined; // []const RowsItem
const Result = struct {
    movie_budget: i32,
    movie_votes: i32,
    movie_title: i32,
};
const result = Result{
    .movie_budget = _min_int(blk1: { var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator); for (rows) |r| { _tmp3.append(r.budget) catch |err| handleError(err); } const _tmp4 = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk1 _tmp4; }),
    .movie_votes = _min_int(blk2: { var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator); for (rows) |r| { _tmp5.append(r.votes) catch |err| handleError(err); } const _tmp6 = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk2 _tmp6; }),
    .movie_title = _min_int(blk3: { var _tmp7 = std.ArrayList(i32).init(std.heap.page_allocator); for (rows) |r| { _tmp7.append(r.title) catch |err| handleError(err); } const _tmp8 = _tmp7.toOwnedSlice() catch |err| handleError(err); break :blk3 _tmp8; }),
}; // Result

fn test_Q18_finds_minimal_budget__votes_and_title_for_Tim_productions() void {
    expect((result == Result{
    .movie_budget = 90,
    .movie_votes = 400,
    .movie_title = "Alpha",
}));
}

pub fn main() void {
    rows = blk0: { var _tmp1 = std.ArrayList(ResultStruct0).init(std.heap.page_allocator); for (cast_info) |ci| { for (name) |n| { if (!((n.id == ci.person_id))) continue; for (title) |t| { if (!((t.id == ci.movie_id))) continue; for (movie_info) |mi| { if (!((mi.movie_id == t.id))) continue; for (movie_info_idx) |mi_idx| { if (!((mi_idx.movie_id == t.id))) continue; for (info_type) |it1| { if (!((it1.id == mi.info_type_id))) continue; for (info_type) |it2| { if (!((it2.id == mi_idx.info_type_id))) continue; if (!((((((((((_contains_list_string(&[_][]const u8{
    "(producer)",
    "(executive producer)",
}, ci.note) and std.mem.eql(u8, it1.info, "budget")) and std.mem.eql(u8, it2.info, "votes")) and std.mem.eql(u8, n.gender, "m")) and n.name.contains("Tim")) and (t.id == ci.movie_id)) and (ci.movie_id == mi.movie_id)) and (ci.movie_id == mi_idx.movie_id)) and (mi.movie_id == mi_idx.movie_id))))) continue; _tmp1.append(ResultStruct0{
    .budget = mi.info,
    .votes = mi_idx.info,
    .title = t.title,
}) catch |err| handleError(err); } } } } } } } const _tmp2 = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk0 _tmp2; };
    _json(result);
    test_Q18_finds_minimal_budget__votes_and_title_for_Tim_productions();
}
