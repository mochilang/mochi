public class Main {
    static class SMAValue {
        double value;
        boolean ok;
        SMAValue(double value, boolean ok) {
            this.value = value;
            this.ok = ok;
        }
        SMAValue() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'ok': %s}", String.valueOf(value), String.valueOf(ok));
        }
    }

    static double[] data = ((double[])(new double[]{(double)(10.0), (double)(12.0), (double)(15.0), (double)(13.0), (double)(14.0), (double)(16.0), (double)(18.0), (double)(17.0), (double)(19.0), (double)(21.0)}));
    static java.math.BigInteger window_size = java.math.BigInteger.valueOf(3);
    static SMAValue[] sma_values;
    static java.math.BigInteger idx = java.math.BigInteger.valueOf(0);

    static SMAValue[] simple_moving_average(double[] data, java.math.BigInteger window_size) {
        if (window_size.compareTo(java.math.BigInteger.valueOf(1)) < 0) {
            throw new RuntimeException(String.valueOf("Window size must be a positive integer"));
        }
        SMAValue[] result_1 = ((SMAValue[])(new SMAValue[]{}));
        double window_sum_1 = (double)(0.0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(data.length))) < 0) {
            window_sum_1 = (double)((double)(window_sum_1) + (double)(data[_idx((data).length, ((java.math.BigInteger)(i_1)).longValue())]));
            if (i_1.compareTo(window_size) >= 0) {
                window_sum_1 = (double)((double)(window_sum_1) - (double)(data[_idx((data).length, ((java.math.BigInteger)(i_1.subtract(window_size))).longValue())]));
            }
            if (i_1.compareTo(window_size.subtract(java.math.BigInteger.valueOf(1))) >= 0) {
                double avg_1 = (double)((double)(window_sum_1) / ((java.math.BigInteger)(window_size)).doubleValue());
                result_1 = ((SMAValue[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new SMAValue((double)(avg_1), true))).toArray(SMAValue[]::new)));
            } else {
                result_1 = ((SMAValue[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new SMAValue((double)(0.0), false))).toArray(SMAValue[]::new)));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((SMAValue[])(result_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            sma_values = ((SMAValue[])(simple_moving_average(((double[])(data)), new java.math.BigInteger(String.valueOf(window_size)))));
            while (idx.compareTo(new java.math.BigInteger(String.valueOf(sma_values.length))) < 0) {
                SMAValue item = sma_values[_idx((sma_values).length, ((java.math.BigInteger)(idx)).longValue())];
                if (item.ok) {
                    System.out.println("Day " + _p(idx.add(java.math.BigInteger.valueOf(1))) + ": " + _p(item.value));
                } else {
                    System.out.println("Day " + _p(idx.add(java.math.BigInteger.valueOf(1))) + ": Not enough data for SMA");
                }
                idx = new java.math.BigInteger(String.valueOf(idx.add(java.math.BigInteger.valueOf(1))));
            }
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
