public class Main {

    static int[] z_function(String s) {
        int[] z = ((int[])(new int[]{}));
        int i = 0;
        while (i < _runeLen(s)) {
            z = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(z), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int l = 0;
        int r = 0;
        i = 1;
        while (i < _runeLen(s)) {
            if (i <= r) {
                int min_edge = r - i + 1;
                int zi = z[i - l];
                if (zi < min_edge) {
                    min_edge = zi;
                }
z[i] = min_edge;
            }
            while (go_next(i, ((int[])(z)), s)) {
z[i] = z[i] + 1;
            }
            if (i + z[i] - 1 > r) {
                l = i;
                r = i + z[i] - 1;
            }
            i = i + 1;
        }
        return z;
    }

    static boolean go_next(int i, int[] z, String s) {
        return i + z[i] < _runeLen(s) && (s.substring(z[i], z[i]+1).equals(s.substring(i + z[i], i + z[i]+1)));
    }

    static int find_pattern(String pattern, String input_str) {
        int answer = 0;
        int[] z_res = ((int[])(z_function(pattern + input_str)));
        int i_1 = 0;
        while (i_1 < z_res.length) {
            if (z_res[i_1] >= _runeLen(pattern)) {
                answer = answer + 1;
            }
            i_1 = i_1 + 1;
        }
        return answer;
    }

    static boolean list_eq_int(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        int i_2 = 0;
        while (i_2 < a.length) {
            if (a[i_2] != b[i_2]) {
                return false;
            }
            i_2 = i_2 + 1;
        }
        return true;
    }

    static void test_z_function() {
        String s1 = "abracadabra";
        int[] expected1 = ((int[])(new int[]{0, 0, 0, 1, 0, 1, 0, 4, 0, 0, 1}));
        int[] r1 = ((int[])(z_function(s1)));
        if (!(Boolean)list_eq_int(((int[])(r1)), ((int[])(expected1)))) {
            throw new RuntimeException(String.valueOf("z_function abracadabra failed"));
        }
        String s2 = "aaaa";
        int[] expected2 = ((int[])(new int[]{0, 3, 2, 1}));
        int[] r2 = ((int[])(z_function(s2)));
        if (!(Boolean)list_eq_int(((int[])(r2)), ((int[])(expected2)))) {
            throw new RuntimeException(String.valueOf("z_function aaaa failed"));
        }
        String s3 = "zxxzxxz";
        int[] expected3 = ((int[])(new int[]{0, 0, 0, 4, 0, 0, 1}));
        int[] r3 = ((int[])(z_function(s3)));
        if (!(Boolean)list_eq_int(((int[])(r3)), ((int[])(expected3)))) {
            throw new RuntimeException(String.valueOf("z_function zxxzxxz failed"));
        }
    }

    static void test_find_pattern() {
        if (find_pattern("abr", "abracadabra") != 2) {
            throw new RuntimeException(String.valueOf("find_pattern abr failed"));
        }
        if (find_pattern("a", "aaaa") != 4) {
            throw new RuntimeException(String.valueOf("find_pattern aaaa failed"));
        }
        if (find_pattern("xz", "zxxzxxz") != 2) {
            throw new RuntimeException(String.valueOf("find_pattern xz failed"));
        }
    }

    static void main() {
        test_z_function();
        test_find_pattern();
        int[] r1_1 = ((int[])(z_function("abracadabra")));
        int[] r2_1 = ((int[])(z_function("aaaa")));
        int[] r3_1 = ((int[])(z_function("zxxzxxz")));
        System.out.println(_p(r1_1));
        System.out.println(_p(r2_1));
        System.out.println(_p(r3_1));
        System.out.println(_p(find_pattern("abr", "abracadabra")));
        System.out.println(_p(find_pattern("a", "aaaa")));
        System.out.println(_p(find_pattern("xz", "zxxzxxz")));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
