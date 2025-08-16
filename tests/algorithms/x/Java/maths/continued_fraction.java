public class Main {

    static long floor_div(long a, long b) {
        long q = Math.floorDiv(((long)(a)), ((long)(b)));
        long r_1 = Math.floorMod(a, b);
        if ((long)(r_1) != 0L && (((long)(a) < 0L && (long)(b) > 0L) || ((long)(a) > 0L && (long)(b) < 0L))) {
            q = (long)((long)(q) - 1L);
        }
        return q;
    }

    static long[] continued_fraction(long numerator, long denominator) {
        long num = (long)(numerator);
        long den_1 = (long)(denominator);
        long[] result_1 = ((long[])(new long[]{}));
        while (true) {
            long integer_part_1 = (long)(floor_div((long)(num), (long)(den_1)));
            result_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result_1), java.util.stream.LongStream.of((long)(integer_part_1))).toArray()));
            num = (long)((long)(num) - (long)((long)(integer_part_1) * (long)(den_1)));
            if ((long)(num) == 0L) {
                break;
            }
            long tmp_1 = (long)(num);
            num = (long)(den_1);
            den_1 = (long)(tmp_1);
        }
        return result_1;
    }

    static String list_to_string(long[] lst) {
        String s = "[";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(lst.length)) {
            s = s + _p(_geti(lst, ((Number)(i_1)).intValue()));
            if ((long)(i_1) < (long)((long)(lst.length) - 1L)) {
                s = s + ", ";
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return s + "]";
    }
    public static void main(String[] args) {
        System.out.println("Continued Fraction of 0.84375 is: " + String.valueOf(list_to_string(((long[])(continued_fraction(27L, 32L))))));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
