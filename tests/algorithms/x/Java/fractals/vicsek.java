public class Main {

    static String repeat_char(String c, int count) {
        String s = "";
        int i = 0;
        while (i < count) {
            s = s + c;
            i = i + 1;
        }
        return s;
    }

    static String[] vicsek(int order) {
        if (order == 0) {
            return new String[]{"#"};
        }
        String[] prev = ((String[])(vicsek(order - 1)));
        int size = prev.length;
        String blank = String.valueOf(repeat_char(" ", size));
        String[] result = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < size) {
            result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(blank + prev[i_1] + blank)).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < size) {
            result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(prev[i_1] + prev[i_1] + prev[i_1])).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < size) {
            result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(blank + prev[i_1] + blank)).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static void print_pattern(String[] pattern) {
        int i_2 = 0;
        while (i_2 < pattern.length) {
            System.out.println(pattern[i_2]);
            i_2 = i_2 + 1;
        }
    }

    static void main() {
        int depth = 3;
        String[] pattern = ((String[])(vicsek(depth)));
        print_pattern(((String[])(pattern)));
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
