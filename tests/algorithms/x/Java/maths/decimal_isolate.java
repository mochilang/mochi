public class Main {

    static double floor(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = (long)((long)(i) - 1L);
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(long n) {
        double p = (double)(1.0);
        long i_2 = 0L;
        while ((long)(i_2) < (long)(n)) {
            p = (double)((double)(p) * (double)(10.0));
            i_2 = (long)((long)(i_2) + 1L);
        }
        return p;
    }

    static double round(double x, long n) {
        double m = (double)(pow10((long)(n)));
        return Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)) / (double)(m);
    }

    static double decimal_isolate(double number, long digit_amount) {
        long whole = (long)(((Number)(number)).intValue());
        double frac_1 = (double)((double)(number) - (double)((((Number)(whole)).doubleValue())));
        if ((long)(digit_amount) > 0L) {
            return round((double)(frac_1), (long)(digit_amount));
        }
        return frac_1;
    }

    static void main() {
        System.out.println(_p(decimal_isolate((double)(1.53), 0L)));
        System.out.println(_p(decimal_isolate((double)(35.345), 1L)));
        System.out.println(_p(decimal_isolate((double)(35.345), 2L)));
        System.out.println(_p(decimal_isolate((double)(35.345), 3L)));
        System.out.println(_p(decimal_isolate((double)(-14.789), 3L)));
        System.out.println(_p(decimal_isolate((double)(0.0), 2L)));
        System.out.println(_p(decimal_isolate((double)(-14.123), 1L)));
        System.out.println(_p(decimal_isolate((double)(-14.123), 2L)));
        System.out.println(_p(decimal_isolate((double)(-14.123), 3L)));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
