// Generated by Mochi compiler v0.10.26 on 1970-01-01T00:00:00Z
const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _concat_string(a: []const u8, b: []const u8) []const u8 {
    return std.mem.concat(u8, &[_][]const u8{ a, b }) catch |err| handleError(err);
}

const Point = struct {
    x: f64,
    y: f64,
    z: f64,
};

const Edge = struct {
    pn1: i32,
    pn2: i32,
    fn1: i32,
    fn2: i32,
    cp: Point,
};

const PointEx = struct {
    p: Point,
    n: i32,
};

fn indexOf(s: []const u8, ch: []const u8) i32 {
    var i = 0; // i32
    while (i < (s).len) {
        if (std.mem.eql(u8, substring(s, i, (i + 1)), ch)) {
            return i;
        }
        i = (i + 1);
    }
    return -1;
}

fn fmt4(x: f64) []const u8 {
    var y = (x * 10000.0); // f64
    if (y >= 0) {
        y = (y + 0.5);
    } else {
        y = (y - 0.5);
    }
    y = (@as(f64, (@as(i32, y))) / 10000.0);
    var s = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{y}) catch |err| handleError(err); // []const u8
    var dot = indexOf(s, "."); // i32
    if (dot == (0 - 1)) {
        s = _concat_string(s, ".0000");
    } else {
        var decs = (((s).len - dot) - 1); // i32
        if (decs > 4) {
            s = substring(s, 0, (dot + 5));
        } else {
            while (decs < 4) {
                s = _concat_string(s, "0");
                decs = (decs + 1);
            }
        }
    }
    if (x >= 0.0) {
        s = _concat_string(" ", s);
    }
    return s;
}

fn fmt2(n: i32) []const u8 {
    const s = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{n}) catch |err| handleError(err); // []const u8
    if ((s).len < 2) {
        return _concat_string(" ", s);
    }
    return s;
}

fn sumPoint(p1: *Point, p2: *Point) Point {
    return Point{
    .x = (p1.x + p2.x),
    .y = (p1.y + p2.y),
    .z = (p1.z + p2.z),
};
}

fn mulPoint(p: *Point, m: f64) Point {
    return Point{
    .x = (p.x * m),
    .y = (p.y * m),
    .z = (p.z * m),
};
}

fn divPoint(p: *Point, d: f64) Point {
    return mulPoint(&p, (1.0 / d));
}

fn centerPoint(p1: *Point, p2: *Point) Point {
    return divPoint(&sumPoint(&p1, &p2), 2.0);
}

fn getFacePoints(points: []const Point, faces: []const []const i32) []const Point {
    var facePoints = std.ArrayList(Point).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i < (faces).len) {
        const face = faces[i]; // []const i32
        var fp = Point{
    .x = 0.0,
    .y = 0.0,
    .z = 0.0,
}; // Point
        for (face) |idx| {
            fp = sumPoint(&fp, &points[idx]);
        }
        fp = divPoint(&fp, (@as(f64, (face).len)));
        facePoints = blk0: { var _tmp0 = std.ArrayList(Point).init(std.heap.page_allocator); defer _tmp0.deinit(); _tmp0.appendSlice(facePoints) catch |err| handleError(err); _tmp0.append(fp) catch |err| handleError(err); break :blk0 _tmp0.items; };
        i = (i + 1);
    }
    return facePoints.items;
}

fn sortEdges(edges: []const []const i32) []const []const i32 {
    var res = std.ArrayList(i32).init(std.heap.page_allocator);
    var tmp = edges; // []const []const i32
    while ((tmp).len > 0) {
        var min = tmp[0]; // []const i32
        var idx = 0; // i32
        var j = 1; // i32
        while (j < (tmp).len) {
            const e = tmp[j]; // []const i32
            if ((e[0] < min[0]) or (((e[0] == min[0]) and (((e[1] < min[1]) or (((e[1] == min[1]) and (e[2] < min[2])))))))) {
                min = e;
                idx = j;
            }
            j = (j + 1);
        }
        res = blk1: { var _tmp1 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp1.deinit(); _tmp1.appendSlice(res) catch |err| handleError(err); _tmp1.append(min) catch |err| handleError(err); break :blk1 _tmp1.items; };
        var out = std.ArrayList(i32).init(std.heap.page_allocator);
        var k = 0; // i32
        while (k < (tmp).len) {
            if (k != idx) {
                out = blk2: { var _tmp2 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp2.deinit(); _tmp2.appendSlice(out) catch |err| handleError(err); _tmp2.append(tmp[k]) catch |err| handleError(err); break :blk2 _tmp2.items; };
            }
            k = (k + 1);
        }
        tmp = out;
    }
    return res.items;
}

fn getEdgesFaces(points: []const Point, faces: []const []const i32) []const Edge {
    var edges = std.ArrayList(i32).init(std.heap.page_allocator);
    var fnum = 0; // i32
    while (fnum < (faces).len) {
        const face = faces[fnum]; // []const i32
        var numP = (face).len; // i32
        var pi = 0; // i32
        while (pi < numP) {
            var pn1 = face[pi]; // i32
            var pn2 = 0; // i32
            if (pi < (numP - 1)) {
                pn2 = face[(pi + 1)];
            } else {
                pn2 = face[0];
            }
            if (pn1 > pn2) {
                var tmpn = pn1; // i32
                pn1 = pn2;
                pn2 = tmpn;
            }
            edges = blk3: { var _tmp3 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp3.deinit(); _tmp3.appendSlice(edges) catch |err| handleError(err); _tmp3.append(&[_]i32{
    pn1,
    pn2,
    fnum,
}) catch |err| handleError(err); break :blk3 _tmp3.items; };
            pi = (pi + 1);
        }
        fnum = (fnum + 1);
    }
    edges = sortEdges(edges);
    var merged = std.ArrayList(i32).init(std.heap.page_allocator);
    var idx = 0; // i32
    while (idx < (edges).len) {
        const e1 = edges[idx]; // []const i32
        if (idx < ((edges).len - 1)) {
            const e2 = edges[(idx + 1)]; // []const i32
            if ((e1[0] == e2[0]) and (e1[1] == e2[1])) {
                merged = blk4: { var _tmp4 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp4.deinit(); _tmp4.appendSlice(merged) catch |err| handleError(err); _tmp4.append(&[_]i32{
    e1[0],
    e1[1],
    e1[2],
    e2[2],
}) catch |err| handleError(err); break :blk4 _tmp4.items; };
                idx = (idx + 2);
                continue;
            }
        }
        merged = blk5: { var _tmp5 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp5.deinit(); _tmp5.appendSlice(merged) catch |err| handleError(err); _tmp5.append(&[_]i32{
    e1[0],
    e1[1],
    e1[2],
    -1,
}) catch |err| handleError(err); break :blk5 _tmp5.items; };
        idx = (idx + 1);
    }
    var edgesCenters = std.ArrayList(Edge).init(std.heap.page_allocator);
    for (merged) |me| {
        const p1 = points[me[0]]; // Point
        const p2 = points[me[1]]; // Point
        const cp = centerPoint(&p1, &p2); // Point
        edgesCenters = blk6: { var _tmp6 = std.ArrayList(Edge).init(std.heap.page_allocator); defer _tmp6.deinit(); _tmp6.appendSlice(edgesCenters) catch |err| handleError(err); _tmp6.append(Edge{
    .pn1 = me[0],
    .pn2 = me[1],
    .fn1 = me[2],
    .fn2 = me[3],
    .cp = cp,
}) catch |err| handleError(err); break :blk6 _tmp6.items; };
    }
    return edgesCenters.items;
}

fn getEdgePoints(points: []const Point, edgesFaces: []const Edge, facePoints: []const Point) []const Point {
    var edgePoints = std.ArrayList(Point).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i < (edgesFaces).len) {
        const edge = edgesFaces[i]; // Edge
        const cp = edge.cp; // Point
        const fp1 = facePoints[edge.fn1]; // Point
        var fp2 = fp1; // Point
        if (edge.fn2 != (0 - 1)) {
            fp2 = facePoints[edge.fn2];
        }
        const cfp = centerPoint(&fp1, &fp2); // Point
        edgePoints = blk7: { var _tmp7 = std.ArrayList(Point).init(std.heap.page_allocator); defer _tmp7.deinit(); _tmp7.appendSlice(edgePoints) catch |err| handleError(err); _tmp7.append(centerPoint(&cp, &cfp)) catch |err| handleError(err); break :blk7 _tmp7.items; };
        i = (i + 1);
    }
    return edgePoints.items;
}

fn getAvgFacePoints(points: []const Point, faces: []const []const i32, facePoints: []const Point) []const Point {
    var numP = (points).len; // i32
    var temp = std.ArrayList(PointEx).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i < numP) {
        temp = blk8: { var _tmp8 = std.ArrayList(PointEx).init(std.heap.page_allocator); defer _tmp8.deinit(); _tmp8.appendSlice(temp) catch |err| handleError(err); _tmp8.append(PointEx{
    .p = Point{
    .x = 0.0,
    .y = 0.0,
    .z = 0.0,
},
    .n = 0,
}) catch |err| handleError(err); break :blk8 _tmp8.items; };
        i = (i + 1);
    }
    var fnum = 0; // i32
    while (fnum < (faces).len) {
        const fp = facePoints[fnum]; // Point
        for (faces[fnum]) |pn| {
            const tp = temp[pn]; // PointEx
            temp.items[pn] = PointEx{
    .p = sumPoint(&tp.p, &fp),
    .n = (tp.n + 1),
};
        }
        fnum = (fnum + 1);
    }
    var avg = std.ArrayList(Point).init(std.heap.page_allocator);
    var j = 0; // i32
    while (j < numP) {
        const tp = temp[j]; // PointEx
        avg = blk9: { var _tmp9 = std.ArrayList(Point).init(std.heap.page_allocator); defer _tmp9.deinit(); _tmp9.appendSlice(avg) catch |err| handleError(err); _tmp9.append(divPoint(&tp.p, @as(f64, tp.n))) catch |err| handleError(err); break :blk9 _tmp9.items; };
        j = (j + 1);
    }
    return avg.items;
}

fn getAvgMidEdges(points: []const Point, edgesFaces: []const Edge) []const Point {
    var numP = (points).len; // i32
    var temp = std.ArrayList(PointEx).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i < numP) {
        temp = blk10: { var _tmp10 = std.ArrayList(PointEx).init(std.heap.page_allocator); defer _tmp10.deinit(); _tmp10.appendSlice(temp) catch |err| handleError(err); _tmp10.append(PointEx{
    .p = Point{
    .x = 0.0,
    .y = 0.0,
    .z = 0.0,
},
    .n = 0,
}) catch |err| handleError(err); break :blk10 _tmp10.items; };
        i = (i + 1);
    }
    for (edgesFaces) |edge| {
        const cp = edge.cp; // Point
        var arr = &[_]i32{
    edge.pn1,
    edge.pn2,
}; // []const i32
        for (arr) |pn| {
            const tp = temp[pn]; // PointEx
            temp.items[pn] = PointEx{
    .p = sumPoint(&tp.p, &cp),
    .n = (tp.n + 1),
};
        }
    }
    var avg = std.ArrayList(Point).init(std.heap.page_allocator);
    var j = 0; // i32
    while (j < numP) {
        const tp = temp[j]; // PointEx
        avg = blk11: { var _tmp11 = std.ArrayList(Point).init(std.heap.page_allocator); defer _tmp11.deinit(); _tmp11.appendSlice(avg) catch |err| handleError(err); _tmp11.append(divPoint(&tp.p, @as(f64, tp.n))) catch |err| handleError(err); break :blk11 _tmp11.items; };
        j = (j + 1);
    }
    return avg.items;
}

fn getPointsFaces(points: []const Point, faces: []const []const i32) []const i32 {
    var pf = std.ArrayList(i32).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i < (points).len) {
        pf = blk12: { var _tmp12 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp12.deinit(); _tmp12.appendSlice(pf) catch |err| handleError(err); _tmp12.append(0) catch |err| handleError(err); break :blk12 _tmp12.items; };
        i = (i + 1);
    }
    var fnum = 0; // i32
    while (fnum < (faces).len) {
        for (faces[fnum]) |pn| {
            pf.items[pn] = (pf[pn] + 1);
        }
        fnum = (fnum + 1);
    }
    return pf.items;
}

fn getNewPoints(points: []const Point, pf: []const i32, afp: []const Point, ame: []const Point) []const Point {
    var newPts = std.ArrayList(Point).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i < (points).len) {
        var n = @as(f64, pf[i]); // f64
        var m1 = (((n - 3.0)) / n); // f64
        var m2 = (1.0 / n); // f64
        var m3 = (2.0 / n); // f64
        const old = points[i]; // Point
        const p1 = mulPoint(&old, m1); // Point
        const p2 = mulPoint(&afp[i], m2); // Point
        const p3 = mulPoint(&ame[i], m3); // Point
        newPts = blk13: { var _tmp13 = std.ArrayList(Point).init(std.heap.page_allocator); defer _tmp13.deinit(); _tmp13.appendSlice(newPts) catch |err| handleError(err); _tmp13.append(sumPoint(&sumPoint(&p1, &p2), &p3)) catch |err| handleError(err); break :blk13 _tmp13.items; };
        i = (i + 1);
    }
    return newPts.items;
}

fn key(a: i32, b: i32) []const u8 {
    if (a < b) {
        return _concat_string(_concat_string(std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{a}) catch |err| handleError(err), ","), std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{b}) catch |err| handleError(err));
    }
    return _concat_string(_concat_string(std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{b}) catch |err| handleError(err), ","), std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{a}) catch |err| handleError(err));
}

fn cmcSubdiv(points: []const Point, faces: []const []const i32) []const i32 {
    const facePoints = getFacePoints(points, faces); // []const Point
    const edgesFaces = getEdgesFaces(points, faces); // []const Edge
    const edgePoints = getEdgePoints(points, edgesFaces, facePoints); // []const Point
    const avgFacePoints = getAvgFacePoints(points, faces, facePoints); // []const Point
    const avgMidEdges = getAvgMidEdges(points, edgesFaces); // []const Point
    const pointsFaces = getPointsFaces(points, faces); // []const i32
    var newPoints = getNewPoints(points, pointsFaces, avgFacePoints, avgMidEdges); // []const Point
    var facePointNums = std.ArrayList(i32).init(std.heap.page_allocator);
    var nextPoint = (newPoints).len; // i32
    for (facePoints) |fp| {
        newPoints = blk14: { var _tmp14 = std.ArrayList(Point).init(std.heap.page_allocator); defer _tmp14.deinit(); _tmp14.appendSlice(newPoints) catch |err| handleError(err); _tmp14.append(fp) catch |err| handleError(err); break :blk14 _tmp14.items; };
        facePointNums = blk15: { var _tmp15 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp15.deinit(); _tmp15.appendSlice(facePointNums) catch |err| handleError(err); _tmp15.append(nextPoint) catch |err| handleError(err); break :blk15 _tmp15.items; };
        nextPoint = (nextPoint + 1);
    }
    var edgePointNums = std.AutoHashMap([]const u8, i32).init(std.heap.page_allocator);
    var idx = 0; // i32
    while (idx < (edgesFaces).len) {
        const e = edgesFaces[idx]; // Edge
        newPoints = blk16: { var _tmp16 = std.ArrayList(Point).init(std.heap.page_allocator); defer _tmp16.deinit(); _tmp16.appendSlice(newPoints) catch |err| handleError(err); _tmp16.append(edgePoints[idx]) catch |err| handleError(err); break :blk16 _tmp16.items; };
        _ = edgePointNums.put(key(e.pn1, e.pn2), nextPoint) catch |err| handleError(err);
        nextPoint = (nextPoint + 1);
        idx = (idx + 1);
    }
    var newFaces = std.ArrayList(i32).init(std.heap.page_allocator);
    var fnum = 0; // i32
    while (fnum < (faces).len) {
        const oldFace = faces[fnum]; // []const i32
        if ((oldFace).len == 4) {
            const a = oldFace[0]; // i32
            const b = oldFace[1]; // i32
            const c = oldFace[2]; // i32
            const d = oldFace[3]; // i32
            const fpnum = facePointNums[fnum]; // i32
            const ab = edgePointNums[key(a, b)]; // i32
            const da = edgePointNums[key(d, a)]; // i32
            const bc = edgePointNums[key(b, c)]; // i32
            const cd = edgePointNums[key(c, d)]; // i32
            newFaces = blk17: { var _tmp17 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp17.deinit(); _tmp17.appendSlice(newFaces) catch |err| handleError(err); _tmp17.append(&[_]i32{
    a,
    ab,
    fpnum,
    da,
}) catch |err| handleError(err); break :blk17 _tmp17.items; };
            newFaces = blk18: { var _tmp18 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp18.deinit(); _tmp18.appendSlice(newFaces) catch |err| handleError(err); _tmp18.append(&[_]i32{
    b,
    bc,
    fpnum,
    ab,
}) catch |err| handleError(err); break :blk18 _tmp18.items; };
            newFaces = blk19: { var _tmp19 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp19.deinit(); _tmp19.appendSlice(newFaces) catch |err| handleError(err); _tmp19.append(&[_]i32{
    c,
    cd,
    fpnum,
    bc,
}) catch |err| handleError(err); break :blk19 _tmp19.items; };
            newFaces = blk20: { var _tmp20 = std.ArrayList([]const i32).init(std.heap.page_allocator); defer _tmp20.deinit(); _tmp20.appendSlice(newFaces) catch |err| handleError(err); _tmp20.append(&[_]i32{
    d,
    da,
    fpnum,
    cd,
}) catch |err| handleError(err); break :blk20 _tmp20.items; };
        }
        fnum = (fnum + 1);
    }
    return [_][]const Point{
    newPoints,
    newFaces,
};
}

fn formatPoint(p: *Point) []const u8 {
    return _concat_string(_concat_string(_concat_string(_concat_string(_concat_string(_concat_string("[", fmt4(p.x)), " "), fmt4(p.y)), " "), fmt4(p.z)), "]");
}

fn formatFace(f: []const i32) []const u8 {
    if ((f).len == 0) {
        return "[]";
    }
    var s = _concat_string("[", fmt2(f[0])); // []const u8
    var i = 1; // i32
    while (i < (f).len) {
        s = _concat_string(_concat_string(s, " "), fmt2(f[i]));
        i = (i + 1);
    }
    s = _concat_string(s, "]");
    return s;
}

fn user_main() void {
    const inputPoints = &[_]Point{
    Point{
    .x = -1.0,
    .y = 1.0,
    .z = 1.0,
},
    Point{
    .x = -1.0,
    .y = -1.0,
    .z = 1.0,
},
    Point{
    .x = 1.0,
    .y = -1.0,
    .z = 1.0,
},
    Point{
    .x = 1.0,
    .y = 1.0,
    .z = 1.0,
},
    Point{
    .x = 1.0,
    .y = -1.0,
    .z = -1.0,
},
    Point{
    .x = 1.0,
    .y = 1.0,
    .z = -1.0,
},
    Point{
    .x = -1.0,
    .y = -1.0,
    .z = -1.0,
},
    Point{
    .x = -1.0,
    .y = 1.0,
    .z = -1.0,
},
}; // []const Point
    const inputFaces = &[_][]const i32{
    &[_]i32{
    0,
    1,
    2,
    3,
},
    &[_]i32{
    3,
    2,
    4,
    5,
},
    &[_]i32{
    5,
    4,
    6,
    7,
},
    &[_]i32{
    7,
    0,
    3,
    5,
},
    &[_]i32{
    7,
    6,
    1,
    0,
},
    &[_]i32{
    6,
    1,
    2,
    4,
},
}; // []const []const i32
    var outputPoints = inputPoints; // []const Point
    var outputFaces = inputFaces; // []const []const i32
    var i = 0; // i32
    while (i < 1) {
        const res = cmcSubdiv(outputPoints, outputFaces); // []const i32
        outputPoints = res[0];
        outputFaces = res[1];
        i = (i + 1);
    }
    for (outputPoints) |p| {
        std.debug.print("{s}\n", .{formatPoint(&p)});
    }
    std.debug.print("\n", .{});
    for (outputFaces) |f| {
        std.debug.print("{s}\n", .{formatFace(f)});
    }
}

pub fn main() void {
    user_main();
}
