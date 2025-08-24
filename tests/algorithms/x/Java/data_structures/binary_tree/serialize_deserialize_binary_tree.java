public class Main {
    interface TreeNode {}

    static class Empty implements TreeNode {
        Empty() {
        }
        Empty() {}
        @Override public String toString() {
            return "Empty{}";
        }
    }

    static class Node implements TreeNode {
        TreeNode left;
        java.math.BigInteger value;
        TreeNode right;
        Node(TreeNode left, java.math.BigInteger value, TreeNode right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'left': %s, 'value': %s, 'right': %s}", String.valueOf(left), String.valueOf(value), String.valueOf(right));
        }
    }

    static class BuildResult {
        TreeNode node;
        java.math.BigInteger next;
        BuildResult(TreeNode node, java.math.BigInteger next) {
            this.node = node;
            this.next = next;
        }
        BuildResult() {}
        @Override public String toString() {
            return String.format("{'node': %s, 'next': %s}", String.valueOf(node), String.valueOf(next));
        }
    }


    static java.math.BigInteger digit(String ch) {
        String digits = "0123456789";
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(digits)))) < 0) {
            if ((_substr(digits, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue())).equals(ch))) {
                return i_1;
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return 0;
    }

    static java.math.BigInteger to_int(String s) {
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger sign_1 = java.math.BigInteger.valueOf(1);
        if (new java.math.BigInteger(String.valueOf(_runeLen(s))).compareTo(java.math.BigInteger.valueOf(0)) > 0 && (_substr(s, (int)(0L), (int)(1L)).equals("-"))) {
            sign_1 = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
            i_2 = java.math.BigInteger.valueOf(1);
        }
        java.math.BigInteger num_1 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            String ch_1 = _substr(s, (int)(((java.math.BigInteger)(i_2)).longValue()), (int)(((java.math.BigInteger)(i_2.add(java.math.BigInteger.valueOf(1)))).longValue()));
            num_1 = new java.math.BigInteger(String.valueOf(num_1.multiply(java.math.BigInteger.valueOf(10)).add(digit(ch_1))));
            i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
        }
        return sign_1.multiply(num_1);
    }

    static String[] split(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        String current_1 = "";
        java.math.BigInteger i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            String ch_3 = _substr(s, (int)(((java.math.BigInteger)(i_4)).longValue()), (int)(((java.math.BigInteger)(i_4.add(java.math.BigInteger.valueOf(1)))).longValue()));
            if ((ch_3.equals(sep))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else {
                current_1 = current_1 + ch_3;
            }
            i_4 = new java.math.BigInteger(String.valueOf(i_4.add(java.math.BigInteger.valueOf(1))));
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        return res;
    }

    static String serialize(TreeNode node) {
        return node instanceof Empty ? "null" : _p(((Node)(node)).value) + "," + String.valueOf(serialize(((Node)(node)).left)) + "," + String.valueOf(serialize(((Node)(node)).right));
    }

    static BuildResult build(String[] nodes, java.math.BigInteger idx) {
        String value = nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        if ((value.equals("null"))) {
            return new BuildResult(new Empty(), idx.add(java.math.BigInteger.valueOf(1)));
        }
        BuildResult left_res_1 = build(((String[])(nodes)), new java.math.BigInteger(String.valueOf(idx.add(java.math.BigInteger.valueOf(1)))));
        BuildResult right_res_1 = build(((String[])(nodes)), new java.math.BigInteger(String.valueOf(left_res_1.next)));
        Node node_1 = new Node(left_res_1.node, to_int(value), right_res_1.node);
        return new BuildResult(node_1, right_res_1.next);
    }

    static TreeNode deserialize(String data) {
        String[] nodes = ((String[])(data.split(java.util.regex.Pattern.quote(","))));
        BuildResult res_2 = build(((String[])(nodes)), java.math.BigInteger.valueOf(0));
        return res_2.node;
    }

    static TreeNode five_tree() {
        Node left_child = new Node(new Empty(), 2, new Empty());
        Node right_left_1 = new Node(new Empty(), 4, new Empty());
        Node right_right_1 = new Node(new Empty(), 5, new Empty());
        Node right_child_1 = new Node(right_left_1, 3, right_right_1);
        return ((TreeNode)(new Node(left_child, 1, right_child_1)));
    }

    static void main() {
        TreeNode root = five_tree();
        String serial_1 = String.valueOf(serialize(root));
        System.out.println(serial_1);
        TreeNode rebuilt_1 = deserialize(serial_1);
        String serial2_1 = String.valueOf(serialize(rebuilt_1));
        System.out.println(serial2_1);
        System.out.println((serial_1.equals(serial2_1)) ? 1 : 0);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
