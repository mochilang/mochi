public class Main {

    static String repeat_char(String ch, int count) {
        String result = "";
        int i = 0;
        while (i < count) {
            result = result + ch;
            i = i + 1;
        }
        return result;
    }

    static String butterfly_pattern(int n) {
        String[] lines = ((String[])(new String[]{}));
        int i_1 = 1;
        while (i_1 < n) {
            String left = String.valueOf(repeat_char("*", i_1));
            String mid = String.valueOf(repeat_char(" ", 2 * (n - i_1) - 1));
            String right = String.valueOf(repeat_char("*", i_1));
            lines = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(left + mid + right)).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        lines = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(repeat_char("*", 2 * n - 1))).toArray(String[]::new)));
        int j = n - 1;
        while (j > 0) {
            String left_1 = String.valueOf(repeat_char("*", j));
            String mid_1 = String.valueOf(repeat_char(" ", 2 * (n - j) - 1));
            String right_1 = String.valueOf(repeat_char("*", j));
            lines = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(left_1 + mid_1 + right_1)).toArray(String[]::new)));
            j = j - 1;
        }
        String out = "";
        int k = 0;
        while (k < lines.length) {
            if (k > 0) {
                out = out + "\n";
            }
            out = out + lines[k];
            k = k + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(butterfly_pattern(3));
            System.out.println(butterfly_pattern(5));
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
