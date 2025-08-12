public class Main {

    static boolean contains(String[] xs, String value) {
        long i = 0;
        while (i < xs.length) {
            if ((xs[(int)(i)].equals(value))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static double jaccard_similarity(String[] set_a, String[] set_b, boolean alternative_union) {
        long intersection_len = 0;
        long i_2 = 0;
        while (i_2 < set_a.length) {
            if (((Boolean)(contains(((String[])(set_b)), set_a[(int)(i_2)])))) {
                intersection_len = intersection_len + 1;
            }
            i_2 = i_2 + 1;
        }
        long union_len_1 = 0;
        if (((Boolean)(alternative_union))) {
            union_len_1 = set_a.length + set_b.length;
        } else {
            String[] union_list_1 = ((String[])(new String[]{}));
            i_2 = 0;
            while (i_2 < set_a.length) {
                String val_a_1 = set_a[(int)(i_2)];
                if (!(Boolean)contains(((String[])(union_list_1)), val_a_1)) {
                    union_list_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(union_list_1), java.util.stream.Stream.of(val_a_1)).toArray(String[]::new)));
                }
                i_2 = i_2 + 1;
            }
            i_2 = 0;
            while (i_2 < set_b.length) {
                String val_b_1 = set_b[(int)(i_2)];
                if (!(Boolean)contains(((String[])(union_list_1)), val_b_1)) {
                    union_list_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(union_list_1), java.util.stream.Stream.of(val_b_1)).toArray(String[]::new)));
                }
                i_2 = i_2 + 1;
            }
            union_len_1 = union_list_1.length;
        }
        return 1.0 * intersection_len / union_len_1;
    }

    static void main() {
        String[] set_a = ((String[])(new String[]{"a", "b", "c", "d", "e"}));
        String[] set_b_1 = ((String[])(new String[]{"c", "d", "e", "f", "h", "i"}));
        System.out.println(jaccard_similarity(((String[])(set_a)), ((String[])(set_b_1)), false));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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
}
