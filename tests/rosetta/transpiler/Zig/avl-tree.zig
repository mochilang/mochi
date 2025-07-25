// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:01:13Z
const std = @import("std");

fn handleError(err: anyerror) noreturn {
    std.debug.panic("{any}", .{err});
}

fn _concat_string(a: []const u8, b: []const u8) []const u8 {
    return std.mem.concat(u8, &[_][]const u8{ a, b }) catch |err| handleError(err);
}

fn Node(data: i32) std.AutoHashMap([]const u8, i32) {
    return struct {
    Data: i32,
    Balance: i32,
    Link: []const i32,
}{
    .Data = data,
    .Balance = 0,
    .Link = &[_]i32{
    0,
    0,
},
};
}

fn getLink(n: std.AutoHashMap([]const u8, i32), dir: i32) i32 {
    return (@as([]const i32, n["Link"]))[dir];
}

fn setLink(n: std.AutoHashMap([]const u8, i32), dir: i32, v: i32) void {
    var links = @as([]const i32, n["Link"]); // []const i32
    links.items[dir] = v;
    _ = n.put("Link", links) catch |err| handleError(err);
}

fn opp(dir: i32) i32 {
    return (1 - dir);
}

fn single(root: std.AutoHashMap([]const u8, i32), dir: i32) std.AutoHashMap([]const u8, i32) {
    var tmp: i32 = getLink(root, opp(dir)); // i32
    setLink(root, opp(dir), getLink(tmp, dir));
    setLink(tmp, dir, root);
    return tmp;
}

fn double(root: std.AutoHashMap([]const u8, i32), dir: i32) std.AutoHashMap([]const u8, i32) {
    var tmp: i32 = getLink(getLink(root, opp(dir)), dir); // i32
    setLink(getLink(root, opp(dir)), dir, getLink(tmp, opp(dir)));
    setLink(tmp, opp(dir), getLink(root, opp(dir)));
    setLink(root, opp(dir), tmp);
    tmp = getLink(root, opp(dir));
    setLink(root, opp(dir), getLink(tmp, dir));
    setLink(tmp, dir, root);
    return tmp;
}

fn adjustBalance(root: std.AutoHashMap([]const u8, i32), dir: i32, bal: i32) void {
    var n = @as(std.AutoHashMap([]const u8, i32), getLink(root, dir)); // std.StringHashMap(i32)
    var nn = @as(std.AutoHashMap([]const u8, i32), getLink(n, opp(dir))); // std.StringHashMap(i32)
    if (nn["Balance"] == 0) {
        _ = root.put("Balance", 0) catch |err| handleError(err);
        _ = n.put("Balance", 0) catch |err| handleError(err);
    } else     if (nn["Balance"] == bal) {
        _ = root.put("Balance", -bal) catch |err| handleError(err);
        _ = n.put("Balance", 0) catch |err| handleError(err);
    } else {
        _ = root.put("Balance", 0) catch |err| handleError(err);
        _ = n.put("Balance", bal) catch |err| handleError(err);
    }
    _ = nn.put("Balance", 0) catch |err| handleError(err);
}

fn insertBalance(root: std.AutoHashMap([]const u8, i32), dir: i32) std.AutoHashMap([]const u8, i32) {
    var n = @as(std.AutoHashMap([]const u8, i32), getLink(root, dir)); // std.StringHashMap(i32)
    var bal = ((2 * dir) - 1); // i32
    if (n["Balance"] == bal) {
        _ = root.put("Balance", 0) catch |err| handleError(err);
        _ = n.put("Balance", 0) catch |err| handleError(err);
        return single(root, opp(dir));
    }
    adjustBalance(root, dir, bal);
    return double(root, opp(dir));
}

fn insertR(root: i32, data: i32) std.AutoHashMap([]const u8, i32) {
    if (root == 0) {
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = Node(data),
    .done = false,
};
    }
    var node = @as(std.AutoHashMap([]const u8, i32), root); // std.StringHashMap(i32)
    var dir = 0; // i32
    if ((@as(i32, node["Data"])) < data) {
        dir = 1;
    }
    var r = insertR(getLink(node, dir), data); // std.StringHashMap(i32)
    setLink(node, dir, r["node"]);
    if (r["done"]) {
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = node,
    .done = true,
};
    }
    _ = node.put("Balance", ((@as(i32, node["Balance"])) + (((2 * dir) - 1)))) catch |err| handleError(err);
    if (node["Balance"] == 0) {
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = node,
    .done = true,
};
    }
    if ((node["Balance"] == 1) or (node["Balance"] == (-1))) {
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = node,
    .done = false,
};
    }
    return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = insertBalance(node, dir),
    .done = true,
};
}

fn Insert(tree: i32, data: i32) i32 {
    const r = insertR(tree, data); // std.StringHashMap(i32)
    return r["node"];
}

fn removeBalance(root: std.AutoHashMap([]const u8, i32), dir: i32) std.AutoHashMap([]const u8, i32) {
    var n = @as(std.AutoHashMap([]const u8, i32), getLink(root, opp(dir))); // std.StringHashMap(i32)
    var bal = ((2 * dir) - 1); // i32
    if (n["Balance"] == (-bal)) {
        _ = root.put("Balance", 0) catch |err| handleError(err);
        _ = n.put("Balance", 0) catch |err| handleError(err);
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = single(root, dir),
    .done = false,
};
    }
    if (n["Balance"] == bal) {
        adjustBalance(root, opp(dir), (-bal));
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = double(root, dir),
    .done = false,
};
    }
    _ = root.put("Balance", -bal) catch |err| handleError(err);
    _ = n.put("Balance", bal) catch |err| handleError(err);
    return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = single(root, dir),
    .done = true,
};
}

fn removeR(root: i32, data: i32) std.AutoHashMap([]const u8, i32) {
    if (root == 0) {
        return struct {
    node: i32,
    done: bool,
}{
    .node = 0,
    .done = false,
};
    }
    var node = @as(std.AutoHashMap([]const u8, i32), root); // std.StringHashMap(i32)
    if ((@as(i32, node["Data"])) == data) {
        if (getLink(node, 0) == 0) {
            return struct {
    node: i32,
    done: bool,
}{
    .node = getLink(node, 1),
    .done = false,
};
        }
        if (getLink(node, 1) == 0) {
            return struct {
    node: i32,
    done: bool,
}{
    .node = getLink(node, 0),
    .done = false,
};
        }
        var heir: i32 = getLink(node, 0); // i32
        while (getLink(heir, 1) != 0) {
            heir = getLink(heir, 1);
        }
        _ = node.put("Data", heir["Data"]) catch |err| handleError(err);
        data = @as(i32, heir["Data"]);
    }
    var dir = 0; // i32
    if ((@as(i32, node["Data"])) < data) {
        dir = 1;
    }
    var r = removeR(getLink(node, dir), data); // std.StringHashMap(i32)
    setLink(node, dir, r["node"]);
    if (r["done"]) {
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = node,
    .done = true,
};
    }
    _ = node.put("Balance", (((@as(i32, node["Balance"])) + 1) - (2 * dir))) catch |err| handleError(err);
    if ((node["Balance"] == 1) or (node["Balance"] == (-1))) {
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = node,
    .done = true,
};
    }
    if (node["Balance"] == 0) {
        return struct {
    node: std.StringHashMap(i32),
    done: bool,
}{
    .node = node,
    .done = false,
};
    }
    return removeBalance(node, dir);
}

fn Remove(tree: i32, data: i32) i32 {
    const r = removeR(tree, data); // std.StringHashMap(i32)
    return r["node"];
}

fn indentStr(n: i32) []const u8 {
    var s = ""; // []const u8
    var i = 0; // i32
    while (i < n) {
        s = _concat_string(s, " ");
        i = (i + 1);
    }
    return s;
}

fn dumpNode(node: i32, indent: i32, comma: bool) void {
    const sp = indentStr(indent); // []const u8
    if (node == 0) {
        var line = _concat_string(sp, "null"); // []const u8
        if (comma) {
            line = _concat_string(line, ",");
        }
        std.debug.print("{s}\n", .{line});
    } else {
        std.debug.print("{s}\n", .{_concat_string(sp, "{")});
        std.debug.print("{any}\n", .{_concat_string(_concat_string(_concat_string(indentStr((indent + 3)), "\"Data\": "), std.fmt.allocPrint(std.heap.page_allocator, "{any}", .{node["Data"]}) catch |err| handleError(err)), ",")});
        std.debug.print("{any}\n", .{_concat_string(_concat_string(_concat_string(indentStr((indent + 3)), "\"Balance\": "), std.fmt.allocPrint(std.heap.page_allocator, "{any}", .{node["Balance"]}) catch |err| handleError(err)), ",")});
        std.debug.print("{s}\n", .{_concat_string(indentStr((indent + 3)), "\"Link\": [")});
        dumpNode(getLink(node, 0), (indent + 6), true);
        dumpNode(getLink(node, 1), (indent + 6), false);
        std.debug.print("{s}\n", .{_concat_string(indentStr((indent + 3)), "]")});
        var end = _concat_string(sp, "}"); // []const u8
        if (comma) {
            end = _concat_string(end, ",");
        }
        std.debug.print("{s}\n", .{end});
    }
}

fn dump(node: i32, indent: i32) void {
    dumpNode(node, indent, false);
}

fn user_main() void {
    var tree: i32 = 0; // i32
    std.debug.print("Empty tree:\n", .{});
    dump(tree, 0);
    std.debug.print("\n", .{});
    std.debug.print("Insert test:\n", .{});
    tree = Insert(tree, 3);
    tree = Insert(tree, 1);
    tree = Insert(tree, 4);
    tree = Insert(tree, 1);
    tree = Insert(tree, 5);
    dump(tree, 0);
    std.debug.print("\n", .{});
    std.debug.print("Remove test:\n", .{});
    tree = Remove(tree, 3);
    tree = Remove(tree, 1);
    var t = @as(std.AutoHashMap([]const u8, i32), tree); // std.StringHashMap(i32)
    _ = t.put("Balance", 0) catch |err| handleError(err);
    tree = t;
    dump(tree, 0);
}

pub fn main() void {
    user_main();
}
