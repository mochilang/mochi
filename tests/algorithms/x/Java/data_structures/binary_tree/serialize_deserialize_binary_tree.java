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
        int value;
        TreeNode right;
        Node(TreeNode left, int value, TreeNode right) {
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
        int next;
        BuildResult(TreeNode node, int next) {
            this.node = node;
            this.next = next;
        }
        BuildResult() {}
        @Override public String toString() {
            return String.format("{'node': %s, 'next': %s}", String.valueOf(node), String.valueOf(next));
        }
    }


    static int digit(String ch) {
        String digits = "0123456789";
        int i = 0;
        while (i < _runeLen(digits)) {
            if ((_substr(digits, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return 0;
    }

    static int to_int(String s) {
        int i_1 = 0;
        int sign = 1;
        if (_runeLen(s) > 0 && (_substr(s, 0, 1).equals("-"))) {
            sign = -1;
            i_1 = 1;
        }
        int num = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            num = num * 10 + digit(ch);
            i_1 = i_1 + 1;
        }
        return sign * num;
    }

    static String[] split(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        String current = "";
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            String ch_1 = _substr(s, i_2, i_2 + 1);
            if ((ch_1.equals(sep))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch_1;
            }
            i_2 = i_2 + 1;
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return res;
    }

    static String serialize(TreeNode node) {
        return node instanceof Empty ? "null" : _p(((Node)(node)).value) + "," + String.valueOf(serialize(((Node)(node)).left)) + "," + String.valueOf(serialize(((Node)(node)).right));
    }

    static BuildResult build(String[] nodes, int idx) {
        String value = nodes[idx];
        if ((value.equals("null"))) {
            return new BuildResult(new Empty(), idx + 1);
        }
        BuildResult left_res = build(((String[])(nodes)), idx + 1);
        BuildResult right_res = build(((String[])(nodes)), left_res.next);
        Node node = new Node(left_res.node, to_int(value), right_res.node);
        return new BuildResult(node, right_res.next);
    }

    static TreeNode deserialize(String data) {
        String[] nodes = ((String[])(data.split(java.util.regex.Pattern.quote(","))));
        BuildResult res_1 = build(((String[])(nodes)), 0);
        return res_1.node;
    }

    static TreeNode five_tree() {
        Node left_child = new Node(new Empty(), 2, new Empty());
        Node right_left = new Node(new Empty(), 4, new Empty());
        Node right_right = new Node(new Empty(), 5, new Empty());
        Node right_child = new Node(right_left, 3, right_right);
        return new Node(left_child, 1, right_child);
    }

    static void main() {
        TreeNode root = five_tree();
        String serial = String.valueOf(serialize(root));
        System.out.println(serial);
        TreeNode rebuilt = deserialize(serial);
        String serial2 = String.valueOf(serialize(rebuilt));
        System.out.println(serial2);
        System.out.println((serial.equals(serial2)) ? 1 : 0);
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
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
        return String.valueOf(v);
    }
}
