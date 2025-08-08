public class Main {
    static class Rate {
        String code;
        double rate;
        Rate(String code, double rate) {
            this.code = code;
            this.rate = rate;
        }
        Rate() {}
        @Override public String toString() {
            return String.format("{'code': '%s', 'rate': %s}", String.valueOf(code), String.valueOf(rate));
        }
    }

    static Rate[] rates;
    static double result;

    static double rate_of(String code) {
        for (Rate r : rates) {
            if ((r.code.equals(code))) {
                return r.rate;
            }
        }
        return 0.0;
    }

    static double convert_currency(String from_, String to, double amount) {
        double from_rate = rate_of(from_);
        double to_rate = rate_of(to);
        if (from_rate == 0.0 || to_rate == 0.0) {
            return 0.0;
        }
        double usd = amount / from_rate;
        return usd * to_rate;
    }
    public static void main(String[] args) {
        rates = ((Rate[])(new Rate[]{new Rate("USD", 1.0), new Rate("EUR", 0.9), new Rate("INR", 83.0), new Rate("JPY", 156.0), new Rate("GBP", 0.78)}));
        result = convert_currency("USD", "INR", 10.0);
        System.out.println(_p(result));
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
