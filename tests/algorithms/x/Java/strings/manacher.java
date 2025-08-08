public class Main {

    static String palindromic_string(String input_string) {
        int max_length = 0;
        String new_input_string = "";
        String output_string = "";
        int n = _runeLen(input_string);
        int i = 0;
        while (i < n - 1) {
            new_input_string = new_input_string + _substr(input_string, i, i + 1) + "|";
            i = i + 1;
        }
        new_input_string = new_input_string + _substr(input_string, n - 1, n);
        int left = 0;
        int right = 0;
        int[] length = ((int[])(new int[]{}));
        i = 0;
        int m = _runeLen(new_input_string);
        while (i < m) {
            length = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(length), java.util.stream.IntStream.of(1)).toArray()));
            i = i + 1;
        }
        int start = 0;
        int j = 0;
        while (j < m) {
            int k = 1;
            if (j <= right) {
                int mirror = left + right - j;
                k = Math.floorDiv(length[mirror], 2);
                int diff = right - j + 1;
                if (diff < k) {
                    k = diff;
                }
                if (k < 1) {
                    k = 1;
                }
            }
            while (j - k >= 0 && j + k < m && (_substr(new_input_string, j + k, j + k + 1).equals(_substr(new_input_string, j - k, j - k + 1)))) {
                k = k + 1;
            }
length[j] = 2 * k - 1;
            if (j + k - 1 > right) {
                left = j - k + 1;
                right = j + k - 1;
            }
            if (length[j] > max_length) {
                max_length = length[j];
                start = j;
            }
            j = j + 1;
        }
        String s = _substr(new_input_string, start - Math.floorDiv(max_length, 2), start + Math.floorDiv(max_length, 2) + 1);
        int idx = 0;
        while (idx < _runeLen(s)) {
            String ch = _substr(s, idx, idx + 1);
            if (!(ch.equals("|"))) {
                output_string = output_string + ch;
            }
            idx = idx + 1;
        }
        return output_string;
    }

    static void main() {
        System.out.println(palindromic_string("abbbaba"));
        System.out.println(palindromic_string("ababa"));
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
