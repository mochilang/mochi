public class Main {
    static class Fibonacci {
        int[] sequence;
        Fibonacci(int[] sequence) {
            this.sequence = sequence;
        }
        Fibonacci() {}
        @Override public String toString() {
            return String.format("{'sequence': %s}", String.valueOf(sequence));
        }
    }

    static class FibGetResult {
        Fibonacci fib;
        int[] values;
        FibGetResult(Fibonacci fib, int[] values) {
            this.fib = fib;
            this.values = values;
        }
        FibGetResult() {}
        @Override public String toString() {
            return String.format("{'fib': %s, 'values': %s}", String.valueOf(fib), String.valueOf(values));
        }
    }


    static Fibonacci create_fibonacci() {
        return new Fibonacci(new int[]{0, 1});
    }

    static FibGetResult fib_get(Fibonacci f, int index) {
        int[] seq = ((int[])(f.sequence));
        while (seq.length < index) {
            int next = seq[seq.length - 1] + seq[seq.length - 2];
            seq = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(seq), java.util.stream.IntStream.of(next)).toArray()));
        }
f.sequence = seq;
        int[] result = ((int[])(new int[]{}));
        int i = 0;
        while (i < index) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(seq[i])).toArray()));
            i = i + 1;
        }
        return new FibGetResult(f, result);
    }

    static void main() {
        Fibonacci fib = create_fibonacci();
        FibGetResult res = fib_get(fib, 10);
        fib = res.fib;
        System.out.println(_p(res.values));
        res = fib_get(fib, 5);
        fib = res.fib;
        System.out.println(_p(res.values));
    }
    public static void main(String[] args) {
        main();
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
