public class Main {
    static String sample = "1011001";
    static String decompressed;

    static boolean list_contains(String[] xs, String v) {
        java.math.BigInteger i = java.math.BigInteger.valueOf(0);
        while (i.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            if ((xs[(int)(((java.math.BigInteger)(i)).longValue())].equals(v))) {
                return true;
            }
            i = new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1))));
        }
        return false;
    }

    static boolean is_power_of_two(java.math.BigInteger n) {
        if (n.compareTo(java.math.BigInteger.valueOf(1)) < 0) {
            return false;
        }
        java.math.BigInteger x_1 = new java.math.BigInteger(String.valueOf(n));
        while (x_1.compareTo(java.math.BigInteger.valueOf(1)) > 0) {
            if (x_1.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(0)) != 0) {
                return false;
            }
            x_1 = new java.math.BigInteger(String.valueOf(x_1.divide(java.math.BigInteger.valueOf(2))));
        }
        return true;
    }

    static String bin_string(java.math.BigInteger n) {
        if (n.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return "0";
        }
        String res_1 = "";
        java.math.BigInteger x_3 = new java.math.BigInteger(String.valueOf(n));
        while (x_3.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger bit_1 = new java.math.BigInteger(String.valueOf(x_3.remainder(java.math.BigInteger.valueOf(2))));
            res_1 = _p(bit_1) + res_1;
            x_3 = new java.math.BigInteger(String.valueOf(x_3.divide(java.math.BigInteger.valueOf(2))));
        }
        return res_1;
    }

    static String decompress_data(String data_bits) {
        java.util.Map<String,String> lexicon = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("0", "0"), java.util.Map.entry("1", "1")))));
        String[] keys_1 = ((String[])(new String[]{"0", "1"}));
        String result_1 = "";
        String curr_string_1 = "";
        java.math.BigInteger index_1 = java.math.BigInteger.valueOf(2);
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(data_bits)))) < 0) {
            curr_string_1 = curr_string_1 + _substr(data_bits, (int)(((java.math.BigInteger)(i_2)).longValue()), (int)(((java.math.BigInteger)(i_2.add(java.math.BigInteger.valueOf(1)))).longValue()));
            if (!(Boolean)list_contains(((String[])(keys_1)), curr_string_1)) {
                i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
                continue;
            }
            String last_match_id_1 = ((String)(lexicon).get(curr_string_1));
            result_1 = result_1 + last_match_id_1;
lexicon.put(curr_string_1, last_match_id_1 + "0");
            if (is_power_of_two(new java.math.BigInteger(String.valueOf(index_1)))) {
                java.util.Map<String,String> new_lex_1 = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
                String[] new_keys_1 = ((String[])(new String[]{}));
                java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
                while (j_1.compareTo(new java.math.BigInteger(String.valueOf(keys_1.length))) < 0) {
                    String curr_key_1 = keys_1[(int)(((java.math.BigInteger)(j_1)).longValue())];
new_lex_1.put("0" + curr_key_1, ((String)(lexicon).get(curr_key_1)));
                    new_keys_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_keys_1), java.util.stream.Stream.of("0" + curr_key_1)).toArray(String[]::new)));
                    j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
                }
                lexicon = new_lex_1;
                keys_1 = ((String[])(new_keys_1));
            }
            String new_key_1 = String.valueOf(bin_string(new java.math.BigInteger(String.valueOf(index_1))));
lexicon.put(new_key_1, last_match_id_1 + "1");
            keys_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_1), java.util.stream.Stream.of(new_key_1)).toArray(String[]::new)));
            index_1 = new java.math.BigInteger(String.valueOf(index_1.add(java.math.BigInteger.valueOf(1))));
            curr_string_1 = "";
            i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
        }
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            decompressed = String.valueOf(decompress_data(sample));
            System.out.println(decompressed);
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

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
