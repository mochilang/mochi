public class Main {
    static int r;

    static int josephus_recursive(int num_people, int step_size) {
        if (num_people <= 0 || step_size <= 0) {
            throw new RuntimeException(String.valueOf("num_people or step_size is not a positive integer."));
        }
        if (num_people == 1) {
            return 0;
        }
        return Math.floorMod((josephus_recursive(num_people - 1, step_size) + step_size), num_people);
    }

    static int find_winner(int num_people, int step_size) {
        return josephus_recursive(num_people, step_size) + 1;
    }

    static int[] remove_at(int[] xs, int idx) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < xs.length) {
            if (i != idx) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            }
            i = i + 1;
        }
        return res;
    }

    static int josephus_iterative(int num_people, int step_size) {
        if (num_people <= 0 || step_size <= 0) {
            throw new RuntimeException(String.valueOf("num_people or step_size is not a positive integer."));
        }
        int[] circle = ((int[])(new int[]{}));
        int i_1 = 1;
        while (i_1 <= num_people) {
            circle = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(circle), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        int current = 0;
        while (circle.length > 1) {
            current = Math.floorMod((current + step_size - 1), circle.length);
            circle = ((int[])(remove_at(((int[])(circle)), current)));
        }
        return circle[0];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r = josephus_recursive(7, 3);
            System.out.println(_p(r));
            System.out.println(_p(find_winner(7, 3)));
            System.out.println(_p(josephus_iterative(7, 3)));
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
