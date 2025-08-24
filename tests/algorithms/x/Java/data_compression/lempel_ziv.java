public class Main {
    static String data = "01001100100111";

    static String to_binary(java.math.BigInteger n) {
        if (n.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return "0";
        }
        java.math.BigInteger num_1 = new java.math.BigInteger(String.valueOf(n));
        String res_1 = "";
        while (num_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger bit_1 = new java.math.BigInteger(String.valueOf(num_1.remainder(java.math.BigInteger.valueOf(2))));
            res_1 = _p(bit_1) + res_1;
            num_1 = new java.math.BigInteger(String.valueOf(num_1.divide(java.math.BigInteger.valueOf(2))));
        }
        return res_1;
    }

    static boolean contains_key_int(java.util.Map<String,java.math.BigInteger> m, String key) {
        for (String k : new java.util.ArrayList<>(m.keySet())) {
            if ((k.equals(key))) {
                return true;
            }
        }
        return false;
    }

    static String lzw_compress(String bits) {
        java.util.Map<String,java.math.BigInteger> dict = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>(java.util.Map.ofEntries(java.util.Map.entry("0", java.math.BigInteger.valueOf(0)), java.util.Map.entry("1", java.math.BigInteger.valueOf(1))))));
        String current_1 = "";
        String result_1 = "";
        java.math.BigInteger index_1 = java.math.BigInteger.valueOf(2);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(bits)))) < 0) {
            String ch_1 = bits.substring((int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1)).longValue())+1);
            String candidate_1 = current_1 + ch_1;
            if (contains_key_int(dict, candidate_1)) {
                current_1 = candidate_1;
            } else {
                result_1 = result_1 + String.valueOf(to_binary(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(dict).get(current_1))))));
dict.put(candidate_1, new java.math.BigInteger(String.valueOf(index_1)));
                index_1 = new java.math.BigInteger(String.valueOf(index_1.add(java.math.BigInteger.valueOf(1))));
                current_1 = ch_1;
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        if (!(current_1.equals(""))) {
            result_1 = result_1 + String.valueOf(to_binary(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(dict).get(current_1))))));
        }
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(lzw_compress(data));
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
