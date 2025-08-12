public class Main {

    static long[] collatz_sequence(long n) {
        if (n < 1) {
            throw new RuntimeException(String.valueOf("Sequence only defined for positive integers"));
        }
        long[] seq_1 = ((long[])(new long[]{n}));
        long current_1 = n;
        while (current_1 != 1) {
            if (Math.floorMod(current_1, 2) == 0) {
                current_1 = Math.floorDiv(current_1, 2);
            } else {
                current_1 = 3 * current_1 + 1;
            }
            seq_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(seq_1), java.util.stream.LongStream.of(current_1)).toArray()));
        }
        return seq_1;
    }

    static void main() {
        long n = 11;
        long[] seq_3 = ((long[])(collatz_sequence(n)));
        System.out.println(_p(seq_3));
        System.out.println("Collatz sequence from " + _p(n) + " took " + _p(seq_3.length) + " steps.");
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
