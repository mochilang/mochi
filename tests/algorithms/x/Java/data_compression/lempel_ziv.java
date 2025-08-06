public class Main {
    static String data;

    static String to_binary(int n) {
        if (n == 0) {
            return "0";
        }
        int num = n;
        String res = "";
        while (num > 0) {
            int bit = Math.floorMod(num, 2);
            res = _p(bit) + res;
            num = num / 2;
        }
        return res;
    }

    static boolean contains_key_int(java.util.Map<String,Integer> m, String key) {
        for (var k : new java.util.ArrayList<>(m.keySet())) {
            if (((Number)(k)).intValue() == key) {
                return true;
            }
        }
        return false;
    }

    static String lzw_compress(String bits) {
        java.util.Map<String,Integer> dict = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1)))));
        String current = "";
        String result = "";
        int index = 2;
        int i = 0;
        while (i < _runeLen(bits)) {
            String ch = bits.substring(i, i+1);
            String candidate = current + ch;
            if (((Boolean)(contains_key_int(dict, candidate)))) {
                current = candidate;
            } else {
                result = result + String.valueOf(to_binary((int)(((int)(dict).getOrDefault(current, 0)))));
dict.put(candidate, index);
                index = index + 1;
                current = ch;
            }
            i = i + 1;
        }
        if (!(current.equals(""))) {
            result = result + String.valueOf(to_binary((int)(((int)(dict).getOrDefault(current, 0)))));
        }
        return result;
    }
    public static void main(String[] args) {
        data = "01001100100111";
        System.out.println(lzw_compress(data));
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
        return String.valueOf(v);
    }
}
