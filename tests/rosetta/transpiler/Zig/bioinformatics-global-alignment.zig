// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:01:16Z
const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _slice_string(s: []const u8, start: i32, end: i32, step: i32) []const u8 {
    var sidx = start;
    var eidx = end;
    var stp = step;
    const n: i32 = @as(i32, @intCast(s.len));
    if (sidx < 0) sidx += n;
    if (eidx < 0) eidx += n;
    if (stp == 0) stp = 1;
    if (sidx < 0) sidx = 0;
    if (eidx > n) eidx = n;
    if (stp > 0 and eidx < sidx) eidx = sidx;
    if (stp < 0 and eidx > sidx) eidx = sidx;
    var res = std.ArrayList(u8).init(std.heap.page_allocator);
    defer res.deinit();
    var i: i32 = sidx;
    while ((stp > 0 and i < eidx) or (stp < 0 and i > eidx)) : (i += stp) {
        res.append(s[@as(usize, @intCast(i))]) catch |err| handleError(err);
    }
    return res.toOwnedSlice() catch |err| handleError(err);
}

fn _concat_string(a: []const u8, b: []const u8) []const u8 {
    return std.mem.concat(u8, &[_][]const u8{ a, b }) catch |err| handleError(err);
}

fn padLeft(s: []const u8, w: i32) []const u8 {
    var res = ""; // []const u8
    var n = (w - @as(i32, @intCast((s).len))); // i32
    while (n > 0) {
        res = _concat_string(res, " ");
        n = (n - 1);
    }
    return _concat_string(res, s);
}

fn indexOfFrom(s: []const u8, ch: []const u8, start: i32) i32 {
    var i = start; // i32
    while (i < @as(i32, @intCast((s).len))) {
        if (std.mem.eql(u8, _slice_string(s, i, (i + 1), 1), ch)) {
            return i;
        }
        i = (i + 1);
    }
    return -1;
}

fn containsStr(s: []const u8, sub: []const u8) bool {
    var i = 0; // i32
    const sl = @as(i32, @intCast((s).len)); // i32
    const subl = @as(i32, @intCast((sub).len)); // i32
    while (i <= (sl - subl)) {
        if (std.mem.eql(u8, _slice_string(s, i, (i + subl), 1), sub)) {
            return true;
        }
        i = (i + 1);
    }
    return false;
}

fn distinct(slist: []const []const u8) []const []const u8 {
    var res = std.ArrayList(u8).init(std.heap.page_allocator);
    for (slist) |s| {
        var found = false; // bool
        for (res) |r| {
            if (std.mem.eql(u8, r, s)) {
                found = true;
                break;
            }
        }
        if (!found) {
            res = blk0: { var _tmp0 = std.ArrayList([]const u8).init(std.heap.page_allocator); defer _tmp0.deinit(); _tmp0.appendSlice(res) catch |err| handleError(err); _tmp0.append(s) catch |err| handleError(err); const res = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 res; };
        }
    }
    return res.items;
}

fn permutations(xs: []const []const u8) []const []const []const u8 {
    if (@as(i32, @intCast((xs).len)) <= 1) {
        return [_][]const []const u8{xs};
    }
    var res = std.ArrayList([]const u8).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i < @as(i32, @intCast((xs).len))) {
        var rest = std.ArrayList(u8).init(std.heap.page_allocator);
        var j = 0; // i32
        while (j < @as(i32, @intCast((xs).len))) {
            if (j != i) {
                rest = blk1: { var _tmp1 = std.ArrayList([]const u8).init(std.heap.page_allocator); defer _tmp1.deinit(); _tmp1.appendSlice(rest) catch |err| handleError(err); _tmp1.append(xs[j]) catch |err| handleError(err); const res = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk1 res; };
            }
            j = (j + 1);
        }
        const subs = permutations(rest); // []const []const []const u8
        for (subs) |p| {
            var perm: []const []const u8 = &[_][]const u8{xs[i]}; // []const []const u8
            var k = 0; // i32
            while (k < @as(i32, @intCast((p).len))) {
                perm = blk2: { var _tmp2 = std.ArrayList([]const u8).init(std.heap.page_allocator); defer _tmp2.deinit(); _tmp2.appendSlice(perm) catch |err| handleError(err); _tmp2.append(p[k]) catch |err| handleError(err); const res = _tmp2.toOwnedSlice() catch |err| handleError(err); break :blk2 res; };
                k = (k + 1);
            }
            res = blk3: { var _tmp3 = std.ArrayList([]const []const u8).init(std.heap.page_allocator); defer _tmp3.deinit(); _tmp3.appendSlice(res) catch |err| handleError(err); _tmp3.append(perm) catch |err| handleError(err); const res = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk3 res; };
        }
        i = (i + 1);
    }
    return res.items;
}

fn headTailOverlap(s1: []const u8, s2: []const u8) i32 {
    var start = 0; // i32
    while (true) {
        const ix = indexOfFrom(s1, s2[0..1], start); // i32
        if (ix == (0 - 1)) {
            return 0;
        }
        start = ix;
        if (s2[0..@as(usize, @intCast(@as(i32, @intCast((s1).len)) - start))] == _slice_string(s1, start, @as(i32, @intCast((s1).len)), 1)) {
            return (@as(i32, @intCast((s1).len)) - start);
        }
        start = (start + 1);
    }
}

fn deduplicate(slist: []const []const u8) []const []const u8 {
    const arr = distinct(slist); // []const i32
    var filtered = std.ArrayList(u8).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i < @as(i32, @intCast((arr).len))) {
        const s1: i32 = arr[i]; // i32
        var within = false; // bool
        var j = 0; // i32
        while (j < @as(i32, @intCast((arr).len))) {
            if ((j != i) and containsStr(arr[j], s1)) {
                within = true;
                break;
            }
            j = (j + 1);
        }
        if (!within) {
            filtered = blk4: { var _tmp4 = std.ArrayList([]const u8).init(std.heap.page_allocator); defer _tmp4.deinit(); _tmp4.appendSlice(filtered) catch |err| handleError(err); _tmp4.append(s1) catch |err| handleError(err); const res = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk4 res; };
        }
        i = (i + 1);
    }
    return filtered.items;
}

fn joinAll(ss: []const []const u8) []const u8 {
    var out = ""; // []const u8
    for (ss) |s| {
        out = _concat_string(out, s);
    }
    return out;
}

fn shortestCommonSuperstring(slist: []const []const u8) []const u8 {
    const ss = deduplicate(slist); // []const []const u8
    var shortest = joinAll(ss); // []const u8
    const perms = permutations(ss); // []const []const []const u8
    var idx = 0; // i32
    while (idx < @as(i32, @intCast((perms).len))) {
        const perm = perms[idx]; // []const []const u8
        var sup = perm[0]; // []const u8
        var i = 0; // i32
        while (i < (@as(i32, @intCast((ss).len)) - 1)) {
            const ov = headTailOverlap(perm[i], perm[(i + 1)]); // i32
            sup = _concat_string(sup, _slice_string(perm[(i + 1)], ov, @as(i32, @intCast((perm[(i + 1)]).len)), 1));
            i = (i + 1);
        }
        if (@as(i32, @intCast((sup).len)) < @as(i32, @intCast((shortest).len))) {
            shortest = sup;
        }
        idx = (idx + 1);
    }
    return shortest;
}

fn printCounts(seq: []const u8) void {
    var a = 0; // i32
    var c = 0; // i32
    var g = 0; // i32
    var t = 0; // i32
    var i = 0; // i32
    while (i < @as(i32, @intCast((seq).len))) {
        const ch = _slice_string(seq, i, (i + 1), 1); // []const u8
        if (std.mem.eql(u8, ch, "A")) {
            a = (a + 1);
        } else {
            if (std.mem.eql(u8, ch, "C")) {
                c = (c + 1);
            } else {
                if (std.mem.eql(u8, ch, "G")) {
                    g = (g + 1);
                } else {
                    if (std.mem.eql(u8, ch, "T")) {
                        t = (t + 1);
                    }
                }
            }
        }
        i = (i + 1);
    }
    const total = @as(i32, @intCast((seq).len)); // i32
    std.debug.print("{s}\n", .{_concat_string(_concat_string("\nNucleotide counts for ", seq), ":\n")});
    std.debug.print("{s}\n", .{(padLeft("A", 10) + padLeft(std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{a}) catch |err| handleError(err), 12))});
    std.debug.print("{s}\n", .{(padLeft("C", 10) + padLeft(std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{c}) catch |err| handleError(err), 12))});
    std.debug.print("{s}\n", .{(padLeft("G", 10) + padLeft(std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{g}) catch |err| handleError(err), 12))});
    std.debug.print("{s}\n", .{(padLeft("T", 10) + padLeft(std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{t}) catch |err| handleError(err), 12))});
    std.debug.print("{s}\n", .{(padLeft("Other", 10) + padLeft(std.fmt.allocPrint(std.heap.page_allocator, "{any}", .{(total - ((((a + c) + g) + t)))}) catch |err| handleError(err), 12))});
    std.debug.print("  ____________________\n", .{});
    std.debug.print("{s}\n", .{(padLeft("Total length", 14) + padLeft(std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{total}) catch |err| handleError(err), 8))});
}

fn user_main() void {
    const tests: []const []const []const u8 = &[_][]const []const u8{
    &[_][]const u8{
    "TA",
    "AAG",
    "TA",
    "GAA",
    "TA",
},
    &[_][]const u8{
    "CATTAGGG",
    "ATTAG",
    "GGG",
    "TA",
},
    &[_][]const u8{
    "AAGAUGGA",
    "GGAGCGCAUC",
    "AUCGCAAUAAGGA",
},
    &[_][]const u8{
    "ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT",
    "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT",
    "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA",
    "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC",
    "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT",
    "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC",
    "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT",
    "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC",
    "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC",
    "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT",
    "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC",
    "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA",
    "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA",
},
}; // []const []const []const u8
    for (tests) |seqs| {
        const scs = shortestCommonSuperstring(seqs); // []const u8
        printCounts(scs);
    }
}

pub fn main() void {
    user_main();
}
