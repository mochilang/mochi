const std = @import("std");

fn AstEmitter(comptime Stream: type) type {
    return struct {
        ast: *std.zig.Ast,
        ws: Stream,

        const EmitError = Stream.Error;
        const Self = @This();

        pub fn init(ws: Stream, ast: *std.zig.Ast) @This() {
            return @This(){
                .ws = ws,
                .ast = ast,
            };
        }

        fn write(self: *Self, v: anytype) EmitError!void {
            try self.ws.write(v);
        }

        fn objectField(self: *Self, f: []const u8) EmitError!void {
            try self.ws.objectField(f);
        }

        fn beginObject(self: *Self) EmitError!void {
            try self.ws.beginObject();
        }

        fn endObject(self: *Self) EmitError!void {
            try self.ws.endObject();
        }

        fn beginArray(self: *Self) EmitError!void {
            try self.ws.beginArray();
        }

        fn endArray(self: *Self) EmitError!void {
            try self.ws.endArray();
        }

        pub fn emitRoot(self: *Self) EmitError!void {
            try self.beginArray();
            for (self.ast.rootDecls()) |idx| {
                try self.emitNode(idx);
            }
            try self.endArray();
        }

        fn visibToken(self: *Self, tok_idx: ?std.zig.Ast.TokenIndex) EmitError!void {
            try self.objectField("visib_token");
            if (tok_idx) |idx| {
                try self.emitToken(null, idx);
            } else {
                try self.write(null);
            }
        }

        fn fnProto(self: *Self, proto: std.zig.Ast.full.FnProto) EmitError!void {
            try self.emitToken("name_token", proto.ast.fn_token + 1);

            try self.objectField("params");
            try self.beginArray();
            var param_it = proto.iterate(self.ast);
            while (param_it.next()) |param| {
                try self.beginObject();
                try self.objectField("name_token");
                if (param.name_token) |nt| {
                    try self.emitToken(null, nt);
                } else {
                    try self.write(null);
                }

                try self.objectField("type_expr");
                try self.emitNode(param.type_expr);

                try self.endObject();
            }
            try self.endArray();

            try self.visibToken(proto.visib_token);
        }

        fn fnDecl(self: *Self, node_idx: std.zig.Ast.Node.Index) EmitError!void {
            var buffer: [1]std.zig.Ast.Node.Index = undefined;
            if (self.ast.fullFnProto(&buffer, node_idx)) |p| {
                try self.fnProto(p);
            }
        }

        fn typeNode(self: *Self, node_idx: std.zig.Ast.Node.Index) EmitError!void {
            try self.objectField("type_node");
            try self.write(@tagName(self.ast.nodes.items(.tag)[node_idx]));
        }

        fn simpleVarDecl(self: *Self, node_idx: std.zig.Ast.Node.Index) EmitError!void {
            const vd = self.ast.simpleVarDecl(node_idx);

            try self.objectField("type_node");
            if (vd.ast.type_node != 0) {
                try self.emitNode(vd.ast.type_node);
            } else {
                try self.write(null);
            }

            try self.visibToken(vd.visib_token);

            try self.objectField("init_node");
            try self.emitNode(vd.ast.init_node);
        }

        fn containerFieldInit(self: *Self, node_idx: std.zig.Ast.Node.Index) EmitError!void {
            const cfi = self.ast.containerFieldInit(node_idx);

            try self.emitToken("main_token", cfi.ast.main_token);

            try self.objectField("type_expr");
            try self.emitNode(cfi.ast.type_expr);

            try self.objectField("value_expr");
            if (cfi.ast.value_expr != 0) {
                try self.emitNode(cfi.ast.value_expr);
            } else {
                try self.write(null);
            }
        }

        fn containerDecl(self: *Self, node_idx: std.zig.Ast.Node.Index) EmitError!void {
            const cd = self.ast.containerDecl(node_idx);

            try self.emitToken("main_token", cd.ast.main_token);

            try self.objectField("members");
            try self.beginArray();
            for (cd.ast.members) |m| {
                try self.emitNode(m);
            }
            try self.endArray();
        }

        fn emitToken(self: *Self, field: ?[]const u8, token_idx: std.zig.Ast.TokenIndex) EmitError!void {
            if (field) |f| {
                try self.objectField(f);
            }
            try self.beginObject();
            try self.objectField("value");
            try self.write(self.ast.tokenSlice(token_idx));
            try self.endObject();
        }

        fn emitNode(self: *Self, node_idx: std.zig.Ast.Node.Index) EmitError!void {
            const n = self.ast.nodes.get(node_idx);
            try self.beginObject();
            try self.objectField("tag");
            try self.write(@tagName(n.tag));
            switch (n.tag) {
                .fn_decl => {
                    try self.fnDecl(node_idx);
                },
                .simple_var_decl => {
                    try self.simpleVarDecl(node_idx);
                },
                .container_decl => {
                    try self.containerDecl(node_idx);
                },
                .container_field_init => {
                    try self.containerFieldInit(node_idx);
                },
                .identifier => {
                    try self.emitToken("main_token", n.main_token);
                },
                .number_literal => {
                    try self.emitToken("main_token", n.main_token);
                },
                else => {
                    // unhandled
                },
            }
            try self.endObject();
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const stdin = std.io.getStdIn();
    var reader = stdin.reader();
    const src_raw = try reader.readAllAlloc(gpa.allocator(), std.math.maxInt(usize));
    defer gpa.allocator().free(src_raw);
    const src = try gpa.allocator().alloc(u8, src_raw.len + 1);
    defer gpa.allocator().free(src);
    std.mem.copyForwards(u8, src[0..src_raw.len], src_raw);
    src[src_raw.len] = 0;

    var ast = std.zig.Ast.parse(gpa.allocator(), src[0..src_raw.len :0], .zig) catch {
        std.log.err("failed to parse source", .{});
        return;
    };
    defer ast.deinit(gpa.allocator());

    const stdout = std.io.getStdOut().writer();
    var ws = std.json.writeStream(stdout, .{ .whitespace = .indent_2 });
    defer ws.deinit();

    var emit = AstEmitter(@TypeOf(ws)).init(ws, &ast);
    try emit.emitRoot();
    try stdout.writeByte('\n');
}
