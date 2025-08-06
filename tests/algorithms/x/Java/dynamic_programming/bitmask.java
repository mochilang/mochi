public class Main {

    static int count_assignments(int person, int[][] task_performed, int[] used) {
        if (person == task_performed.length) {
            return 1;
        }
        int total = 0;
        int[] tasks = ((int[])(task_performed[person]));
        int i = 0;
        while (i < tasks.length) {
            int t = tasks[i];
            if (!(Boolean)(java.util.Arrays.stream(used).anyMatch((x) -> ((Number)(x)).intValue() == t))) {
                total = total + count_assignments(person + 1, ((int[][])(task_performed)), ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(used), java.util.stream.IntStream.of(t)).toArray())));
            }
            i = i + 1;
        }
        return total;
    }

    static int count_no_of_ways(int[][] task_performed) {
        return count_assignments(0, ((int[][])(task_performed)), ((int[])(new int[]{})));
    }

    static void main() {
        int[][] task_performed = ((int[][])(new int[][]{new int[]{1, 3, 4}, new int[]{1, 2, 5}, new int[]{3, 4}}));
        System.out.println(_p(count_no_of_ways(((int[][])(task_performed)))));
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
