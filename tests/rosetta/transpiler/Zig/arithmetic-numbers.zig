// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:00:43Z
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

fn sieve(limit: i32) []const i32 {
    var spf = std.ArrayList(i32).init(std.heap.page_allocator);
    var i = 0; // i32
    while (i <= limit) {
        spf = blk0: { var _tmp0 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp0.deinit(); _tmp0.appendSlice(spf) catch |err| handleError(err); _tmp0.append(0) catch |err| handleError(err); const res = _tmp0.toOwnedSlice() catch |err| handleError(err); break :blk0 res; };
        i = (i + 1);
    }
    i = 2;
    while (i <= limit) {
        if (spf[i] == 0) {
            spf.items[i] = i;
            if ((i * i) <= limit) {
                var j = (i * i); // i32
                while (j <= limit) {
                    if (spf[j] == 0) {
                        spf.items[j] = i;
                    }
                    j = (j + i);
                }
            }
        }
        i = (i + 1);
    }
    return spf.items;
}

fn primesFrom(spf: []const i32, limit: i32) []const i32 {
    var primes = std.ArrayList(i32).init(std.heap.page_allocator);
    var i = 3; // i32
    while (i <= limit) {
        if (spf[i] == i) {
            primes = blk1: { var _tmp1 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp1.deinit(); _tmp1.appendSlice(primes) catch |err| handleError(err); _tmp1.append(i) catch |err| handleError(err); const res = _tmp1.toOwnedSlice() catch |err| handleError(err); break :blk1 res; };
        }
        i = (i + 1);
    }
    return primes.items;
}

fn pad3(n: i32) []const u8 {
    var s = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{n}) catch |err| handleError(err); // []const u8
    while (@as(i32, @intCast((s).len)) < 3) {
        s = _concat_string(" ", s);
    }
    return s;
}

fn commatize(n: i32) []const u8 {
    var s = std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{n}) catch |err| handleError(err); // []const u8
    var out = ""; // []const u8
    var i = (@as(i32, @intCast((s).len)) - 1); // i32
    var c = 0; // i32
    while (i >= 0) {
        out = _concat_string(_slice_string(s, i, (i + 1), 1), out);
        c = (c + 1);
        if ((@mod(c, 3) == 0) and (i > 0)) {
            out = _concat_string(",", out);
        }
        i = (i - 1);
    }
    return out;
}

fn primeCount(primes: []const i32, last: i32, spf: []const i32) i32 {
    var lo = 0; // i32
    var hi = @as(i32, @intCast((primes).len)); // i32
    while (lo < hi) {
        var mid = @as(i32, ((((lo + hi)) / 2))); // i32
        if (primes[mid] < last) {
            lo = (mid + 1);
        } else {
            hi = mid;
        }
    }
    var count = (lo + 1); // i32
    if (spf[last] != last) {
        count = (count - 1);
    }
    return count;
}

fn arithmeticNumbers(limit: i32, spf: []const i32) []const i32 {
    var arr: []const i32 = &[_]i32{1}; // []const i32
    var n = 3; // i32
    while (@as(i32, @intCast((arr).len)) < limit) {
        if (spf[n] == n) {
            arr = blk2: { var _tmp2 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp2.deinit(); _tmp2.appendSlice(arr) catch |err| handleError(err); _tmp2.append(n) catch |err| handleError(err); const res = _tmp2.toOwnedSlice() catch |err| handleError(err); break :blk2 res; };
        } else {
            var x = n; // i32
            var sigma = 1; // i32
            var tau = 1; // i32
            while (x > 1) {
                var p = spf[x]; // i32
                if (p == 0) {
                    p = x;
                }
                var cnt = 0; // i32
                var power = p; // i32
                var sum = 1; // i32
                while (@mod(x, p) == 0) {
                    x = (x / p);
                    cnt = (cnt + 1);
                    sum = (sum + power);
                    power = (power * p);
                }
                sigma = (sigma * sum);
                tau = (tau * ((cnt + 1)));
            }
            if (@mod(sigma, tau) == 0) {
                arr = blk3: { var _tmp3 = std.ArrayList(i32).init(std.heap.page_allocator); defer _tmp3.deinit(); _tmp3.appendSlice(arr) catch |err| handleError(err); _tmp3.append(n) catch |err| handleError(err); const res = _tmp3.toOwnedSlice() catch |err| handleError(err); break :blk3 res; };
            }
        }
        n = (n + 1);
    }
    return arr.items;
}

fn user_main() void {
    const limit = 1228663; // i32
    const spf = sieve(limit); // []const i32
    const primes = primesFrom(spf, limit); // []const i32
    const arr = arithmeticNumbers(1000000, spf); // []const i32
    std.debug.print("The first 100 arithmetic numbers are:\n", .{});
    var i = 0; // i32
    while (i < 100) {
        var line = ""; // []const u8
        var j = 0; // i32
        while (j < 10) {
            line = _concat_string(line, pad3(arr[(i + j)]));
            if (j < 9) {
                line = _concat_string(line, " ");
            }
            j = (j + 1);
        }
        std.debug.print("{s}\n", .{line});
        i = (i + 10);
    }
    for (&[_]i32{
    1000,
    10000,
    100000,
    1000000,
}) |x| {
        const last = arr[(x - 1)]; // i32
        const lastc = commatize(last); // []const u8
        std.debug.print("{s}\n", .{_concat_string(_concat_string(_concat_string("\nThe ", commatize(x)), "th arithmetic number is: "), lastc)});
        const pc = primeCount(primes, last, spf); // i32
        const comp = ((x - pc) - 1); // i32
        std.debug.print("{s}\n", .{_concat_string(_concat_string(_concat_string(_concat_string("The count of such numbers <= ", lastc), " which are composite is "), commatize(comp)), ".")});
    }
}

pub fn main() void {
    user_main();
}
