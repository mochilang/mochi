public class Main {
    static int[] nums = new int[]{1, 2};
    static String[] letters = new String[]{"A", "B"};
    static boolean[] bools = new boolean[]{true, false};
    static java.util.List<Result2> combos = new java.util.ArrayList<Result2>() {{ java.util.ArrayList<Result2> _tmp = new java.util.ArrayList<>(); for (var n : nums) { for (var l : letters) { for (var b : bools) { _tmp.add(new Result2(n, l, b)); } } } java.util.ArrayList<Result2> list = _tmp; java.util.ArrayList<Result2> _res = new java.util.ArrayList<>(); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Result2)list.get(i)); } addAll(_res);}};
    static class Result2 {
        int n;
        String l;
        boolean b;
        Result2(int n, String l, boolean b) {
            this.n = n;
            this.l = l;
            this.b = b;
        }
        @Override public String toString() {
            return String.format("{'n': %s, 'l': '%s', 'b': %s}", q(n), q(l), q(b));
        }
    }


    static String q(Object v) {
        if (v instanceof String) return "'" + v.toString() + "'";
        return String.valueOf(v);
    }

    static String boolStr(Object v) {
        if (v instanceof Boolean b) return b ? "True" : "False";
        return String.valueOf(v);
    }

    public static void main(String[] args) {
        System.out.println(boolStr("--- Cross Join of three lists ---"));
        for (var c : combos) {
            System.out.println(boolStr(c.n) + " " + boolStr(c.l) + " " + boolStr(c.b));
        }
    }
}
