// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:01:19Z
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

var primes = primesUpTo(3200000); // []const i32

fn primesUpTo(n: i32) []const i32 {
    var sieve = std.ArrayList(bool).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i <= n) {
        sieve = blk0: { var _tmp0 = std.ArrayList(bool).init(std.heap.page_allocator); defer _tmp0.deinit(); _tmp0.appendSlice(sieve) catch |err| handleError(err); _tmp0.append(true) catch |err| handleError(err); const res = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 res; };
        i = (i + 1);
    }
    var p = 2; // i32
    while ((p * p) <= n) {
        if (sieve[p]) {
            var m = (p * p); // i32
            while (m <= n) {
                sieve.items[m] = false;
                m = (m + p);
            }
        }
        p = (p + 1);
    }
    var res = std.ArrayList(i32).init(std.heap.page_allocator);
    var x = 2; // i32
    while (x <= n) {
        if (sieve[x]) {
            res = blk1: { var _tmp1 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp1.deinit(); _tmp1.appendSlice(res) catch |err| handleError(err); _tmp1.append(x) catch |err| handleError(err); const res = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk1 res; };
        }
        x = (x + 1);
    }
    return res.items;
}

fn sortInts(xs: []const i32) []const i32 {
    var res = std.ArrayList(i32).init(std.heap.page_allocator);
    var tmp = xs; // []const i32
    while (@as(i32, @intCast((tmp).len)) > 0) {
        var min = tmp[0]; // i32
        var idx = 0; // i32
        var i = 1; // i32
        while (i < @as(i32, @intCast((tmp).len))) {
            if (tmp[i] < min) {
                min = tmp[i];
                idx = i;
            }
            i = (i + 1);
        }
        res = blk2: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp2.deinit(); _tmp2.appendSlice(res) catch |err| handleError(err); _tmp2.append(min) catch |err| handleError(err); const res = _tmp2.toOwnedSlice() catch |err| handleError(err); break :blk2 res; };
        var out = std.ArrayList(i32).init(std.heap.page_allocator);
        var j = 0; // i32
        while (j < @as(i32, @intCast((tmp).len))) {
            if (j != idx) {
                out = blk3: { var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp3.deinit(); _tmp3.appendSlice(out) catch |err| handleError(err); _tmp3.append(tmp[j]) catch |err| handleError(err); const res = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk3 res; };
            }
            j = (j + 1);
        }
        tmp = out;
    }
    return res.items;
}

fn commatize(n: i32) []const u8 {
    var s = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{n}) catch |err| handleError(err); // []const u8
    var i = (@as(i32, @intCast((s).len)) - 3); // i32
    while (i >= 1) {
        s = _concat_string(_concat_string(s[0..@as(usize, @intCast(i))], ","), _slice_string(s, i, @as(i32, @intCast((s).len)), 1));
        i = (i - 3);
    }
    return s;
}

fn getBrilliant(digits: i32, limit: i32, countOnly: bool) std.AutoHashMap([]const u8, i32) {
    var brilliant = std.ArrayList(i32).init(std.heap.page_allocator);
    var count = 0; // i32
    var pow = 1; // i32
    var next = 999999999999999; // i32
    var k = 1; // i32
    while (k <= digits) {
        var s = std.ArrayList(i32).init(std.heap.page_allocator);
        for (primes) |p| {
            if (p >= (pow * 10)) {
                break;
            }
            if (p > pow) {
                s = blk4: { var _tmp4 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp4.deinit(); _tmp4.appendSlice(s) catch |err| handleError(err); _tmp4.append(p) catch |err| handleError(err); const res = _tmp4.toOwnedSlice() catch |err| handleError(err); break :blk4 res; };
            }
        }
        var i = 0; // i32
        while (i < @as(i32, @intCast((s).len))) {
            var j = i; // i32
            while (j < @as(i32, @intCast((s).len))) {
                var prod = (s[i] * s[j]); // i32
                if (prod < limit) {
                    if (countOnly) {
                        count = (count + 1);
                    } else {
                        brilliant = blk5: { var _tmp5 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp5.deinit(); _tmp5.appendSlice(brilliant) catch |err| handleError(err); _tmp5.append(prod) catch |err| handleError(err); const res = _tmp5.toOwnedSlice() catch |err| handleError(err); break :blk5 res; };
                    }
                } else {
                    if (prod < next) {
                        next = prod;
                    }
                    break;
                }
                j = (j + 1);
            }
            i = (i + 1);
        }
        pow = (pow * 10);
        k = (k + 1);
    }
    if (countOnly) {
        return struct {
    bc: fn(i32) i32,
    next: i32,
}{
    .bc = count,
    .next = next,
};
    }
    return struct {
    bc: []const i32,
    next: i32,
}{
    .bc = brilliant,
    .next = next,
};
}

fn user_main() void {
    std.debug.print("First 100 brilliant numbers:\n", .{});
    const r = getBrilliant(2, 10000, false); // std.StringHashMap(i32)
    var br = sortInts(r["bc"]); // []const i32
    br = br[0..100];
    var i = 0; // i32
    while (i < @as(i32, @intCast((br).len))) {
        std.debug.print("{any} {d}\n", .{_concat_string(std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{br[i]}) catch |err| handleError(err)(4, " "), " "), @intFromBool(false)});
        if (@mod(((i + 1)), 10) == 0) {
            std.debug.print(" {d}\n", .{true});
        }
        i = (i + 1);
    }
    std.debug.print(" {d}\n", .{true});
    var k = 1; // i32
    while (k <= 13) {
        const limit: i32 = pow(10, k); // i32
        const r2 = getBrilliant(k, limit, true); // std.StringHashMap(i32)
        const total: i32 = r2["bc"]; // i32
        const next: i32 = r2["next"]; // i32
        const climit = commatize(limit); // []const u8
        const ctotal = commatize((total + 1)); // []const u8
        const cnext = commatize(next); // []const u8
        std.debug.print("{s}\n", .{_concat_string(_concat_string(_concat_string(_concat_string(_concat_string("First >= ", climit.padStart(18, " ")), " is "), ctotal.padStart(14, " ")), " in the series: "), cnext.padStart(18, " "))});
        k = (k + 1);
    }
}

pub fn main() void {
}
