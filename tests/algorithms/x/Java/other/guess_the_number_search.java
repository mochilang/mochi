public class Main {

    static int get_avg(int number_1, int number_2) {
        return Math.floorDiv((number_1 + number_2), 2);
    }

    static int[] guess_the_number(int lower, int higher, int to_guess) {
        if (lower > higher) {
            throw new RuntimeException(String.valueOf("argument value for lower and higher must be(lower > higher)"));
        }
        if (!(lower < to_guess && to_guess < higher)) {
            throw new RuntimeException(String.valueOf("guess value must be within the range of lower and higher value"));
        }
        java.util.function.Function<Integer,String>[] answer = new java.util.function.Function[1];
        answer[0] = (number) -> {
        if (number > to_guess) {
            return "high";
        } else         if (number < to_guess) {
            return "low";
        } else {
            return "same";
        }
};
        System.out.println("started...");
        int last_lowest = lower;
        int last_highest = higher;
        int[] last_numbers = ((int[])(new int[]{}));
        while (true) {
            int number_1 = get_avg(last_lowest, last_highest);
            last_numbers = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(last_numbers), java.util.stream.IntStream.of(number_1)).toArray()));
            String resp = String.valueOf(answer[0].apply(number_1));
            if ((resp.equals("low"))) {
                last_lowest = number_1;
            } else             if ((resp.equals("high"))) {
                last_highest = number_1;
            } else {
                break;
            }
        }
        System.out.println("guess the number : " + _p(_geti(last_numbers, last_numbers.length - 1)));
        System.out.println("details : " + _p(last_numbers));
        return last_numbers;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            guess_the_number(10, 1000, 17);
            guess_the_number(-10000, 10000, 7);
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
