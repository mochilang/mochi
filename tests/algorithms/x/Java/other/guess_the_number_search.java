public class Main {

    static java.math.BigInteger get_avg(java.math.BigInteger number_1, java.math.BigInteger number_2) {
        return new java.math.BigInteger(String.valueOf((number_1.add(number_2)).divide(java.math.BigInteger.valueOf(2))));
    }

    static java.math.BigInteger[] guess_the_number(java.math.BigInteger lower, java.math.BigInteger higher, java.math.BigInteger to_guess) {
        if (lower.compareTo(higher) > 0) {
            throw new RuntimeException(String.valueOf("argument value for lower and higher must be(lower > higher)"));
        }
        if (!(lower.compareTo(to_guess) < 0 && to_guess.compareTo(higher) < 0)) {
            throw new RuntimeException(String.valueOf("guess value must be within the range of lower and higher value"));
        }
        java.util.function.Function<java.math.BigInteger,String>[] answer = new java.util.function.Function[1];
        answer[0] = (number) -> {
        if (number.compareTo(to_guess) > 0) {
            return "high";
        } else         if (number.compareTo(to_guess) < 0) {
            return "low";
        } else {
            return "same";
        }
};
        System.out.println("started...");
        java.math.BigInteger last_lowest_1 = new java.math.BigInteger(String.valueOf(lower));
        java.math.BigInteger last_highest_1 = new java.math.BigInteger(String.valueOf(higher));
        java.math.BigInteger[] last_numbers_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        while (true) {
            java.math.BigInteger number_1 = new java.math.BigInteger(String.valueOf(get_avg(new java.math.BigInteger(String.valueOf(last_lowest_1)), new java.math.BigInteger(String.valueOf(last_highest_1)))));
            last_numbers_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(last_numbers_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(number_1)))).toArray(java.math.BigInteger[]::new)));
            String resp_1 = String.valueOf(answer[0].apply(new java.math.BigInteger(String.valueOf(number_1))));
            if ((resp_1.equals("low"))) {
                last_lowest_1 = new java.math.BigInteger(String.valueOf(number_1));
            } else             if ((resp_1.equals("high"))) {
                last_highest_1 = new java.math.BigInteger(String.valueOf(number_1));
            } else {
                break;
            }
        }
        System.out.println("guess the number : " + _p(_geto(last_numbers_1, ((Number)(new java.math.BigInteger(String.valueOf(last_numbers_1.length)).subtract(java.math.BigInteger.valueOf(1)))).intValue())));
        System.out.println("details : " + _p(last_numbers_1));
        return ((java.math.BigInteger[])(last_numbers_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            guess_the_number(java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(1000), java.math.BigInteger.valueOf(17));
            guess_the_number(new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(10000)).negate())), java.math.BigInteger.valueOf(10000), java.math.BigInteger.valueOf(7));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
