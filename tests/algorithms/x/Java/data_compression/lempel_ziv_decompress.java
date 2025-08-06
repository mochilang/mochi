public class Main {
    static String sample;
    static String decompressed;

    static boolean list_contains(String[] xs, String v) {
        int i = 0;
        while (i < xs.length) {
            if ((xs[i].equals(v))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static boolean is_power_of_two(int n) {
        if (n < 1) {
            return false;
        }
        int x = n;
        while (x > 1) {
            if (Math.floorMod(x, 2) != 0) {
                return false;
            }
            x = x / 2;
        }
        return true;
    }

    static String bin_string(int n) {
        if (n == 0) {
            return "0";
        }
        String res = "";
        int x_1 = n;
        while (x_1 > 0) {
            int bit = Math.floorMod(x_1, 2);
            res = _p(bit) + res;
            x_1 = x_1 / 2;
        }
        return res;
    }

    static String decompress_data(String data_bits) {
        java.util.Map<String,String> lexicon = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("0", "0"), java.util.Map.entry("1", "1")))));
        String[] keys = ((String[])(new String[]{"0", "1"}));
        String result = "";
        String curr_string = "";
        int index = 2;
        int i_1 = 0;
        while (i_1 < _runeLen(data_bits)) {
            curr_string = curr_string + _substr(data_bits, i_1, i_1 + 1);
            if (!(Boolean)list_contains(((String[])(keys)), curr_string)) {
                i_1 = i_1 + 1;
                continue;
            }
            String last_match_id = ((String)(lexicon).get(curr_string));
            result = result + last_match_id;
lexicon.put(curr_string, last_match_id + "0");
            if (((Boolean)(is_power_of_two(index)))) {
                java.util.Map<String,String> new_lex = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
                String[] new_keys = ((String[])(new String[]{}));
                int j = 0;
                while (j < keys.length) {
                    String curr_key = keys[j];
new_lex.put("0" + curr_key, ((String)(lexicon).get(curr_key)));
                    new_keys = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_keys), java.util.stream.Stream.of("0" + curr_key)).toArray(String[]::new)));
                    j = j + 1;
                }
                lexicon = new_lex;
                keys = ((String[])(new_keys));
            }
            String new_key = String.valueOf(bin_string(index));
lexicon.put(new_key, last_match_id + "1");
            keys = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys), java.util.stream.Stream.of(new_key)).toArray(String[]::new)));
            index = index + 1;
            curr_string = "";
            i_1 = i_1 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        sample = "1011001";
        decompressed = String.valueOf(decompress_data(sample));
        System.out.println(decompressed);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
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
        return String.valueOf(v);
    }
}
