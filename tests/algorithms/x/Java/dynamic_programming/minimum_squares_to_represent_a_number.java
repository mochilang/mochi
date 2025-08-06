public class Main {

    static int[] make_list(int len, int value) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < len) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return arr;
    }

    static int int_sqrt(int n) {
        int r = 0;
        while ((r + 1) * (r + 1) <= n) {
            r = r + 1;
        }
        return r;
    }

    static int minimum_squares_to_represent_a_number(int number) {
        if (number < 0) {
            throw new RuntimeException(String.valueOf("the value of input must not be a negative number"));
        }
        if (number == 0) {
            return 1;
        }
        int[] answers = ((int[])(make_list(number + 1, -1)));
answers[0] = 0;
        int i_1 = 1;
        while (i_1 <= number) {
            int answer = i_1;
            int root = int_sqrt(i_1);
            int j = 1;
            while (j <= root) {
                int current_answer = 1 + answers[i_1 - j * j];
                if (current_answer < answer) {
                    answer = current_answer;
                }
                j = j + 1;
            }
answers[i_1] = answer;
            i_1 = i_1 + 1;
        }
        return answers[number];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(minimum_squares_to_represent_a_number(25));
            System.out.println(minimum_squares_to_represent_a_number(21));
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
