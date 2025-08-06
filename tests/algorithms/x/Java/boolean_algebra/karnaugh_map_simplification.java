public class Main {
    static int[][] kmap = new int[0][];

    static String row_string(int[] row) {
        String s = "[";
        int i = 0;
        while (i < row.length) {
            s = s + _p(_geti(row, i));
            if (i < row.length - 1) {
                s = s + ", ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static void print_kmap(int[][] kmap) {
        int i_1 = 0;
        while (i_1 < kmap.length) {
            System.out.println(row_string(((int[])(kmap[i_1]))));
            i_1 = i_1 + 1;
        }
    }

    static String join_terms(String[] terms) {
        if (terms.length == 0) {
            return "";
        }
        String res = terms[0];
        int i_2 = 1;
        while (i_2 < terms.length) {
            res = res + " + " + terms[i_2];
            i_2 = i_2 + 1;
        }
        return res;
    }

    static String simplify_kmap(int[][] board) {
        String[] terms = ((String[])(new String[]{}));
        int a = 0;
        while (a < board.length) {
            int[] row = ((int[])(board[a]));
            int b = 0;
            while (b < row.length) {
                int item = row[b];
                if (item != 0) {
                    String term = String.valueOf(String.valueOf((a != 0 ? "A" : "A'")) + String.valueOf((b != 0 ? "B" : "B'")));
                    terms = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(terms), java.util.stream.Stream.of(term)).toArray(String[]::new)));
                }
                b = b + 1;
            }
            a = a + 1;
        }
        String expr = String.valueOf(join_terms(((String[])(terms))));
        return expr;
    }
    public static void main(String[] args) {
        kmap = ((int[][])(new int[][]{new int[]{0, 1}, new int[]{1, 1}}));
        print_kmap(((int[][])(kmap)));
        System.out.println("Simplified Expression:");
        System.out.println(simplify_kmap(((int[][])(kmap))));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
