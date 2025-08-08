public class Main {
    static class FibPair {
        int fn;
        int fn1;
        FibPair(int fn, int fn1) {
            this.fn = fn;
            this.fn1 = fn1;
        }
        FibPair() {}
        @Override public String toString() {
            return String.format("{'fn': %s, 'fn1': %s}", String.valueOf(fn), String.valueOf(fn1));
        }
    }

    static int i = 0;

    static FibPair _fib(int n) {
        if (n == 0) {
            return new FibPair(0, 1);
        }
        FibPair half = _fib(Math.floorDiv(n, 2));
        int a = half.fn;
        int b = half.fn1;
        int c = a * (b * 2 - a);
        int d = a * a + b * b;
        if (Math.floorMod(n, 2) == 0) {
            return new FibPair(c, d);
        }
        return new FibPair(d, c + d);
    }

    static int fibonacci(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("Negative arguments are not supported"));
        }
        FibPair res = _fib(n);
        return res.fn;
    }
    public static void main(String[] args) {
        i = 0;
        while (i < 13) {
            System.out.println(_p(fibonacci(i)));
            i = i + 1;
        }
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
