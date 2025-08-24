public class Main {
    static String example1 = "AAAABBBCCDAA";
    static String encoded1;
    static String example2 = "A";
    static String encoded2;
    static String example3 = "AAADDDDDDFFFCCCAAVVVV";
    static String encoded3;

    static String run_length_encode(String text) {
        if (new java.math.BigInteger(String.valueOf(_runeLen(text))).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return "";
        }
        String encoded_1 = "";
        java.math.BigInteger count_1 = java.math.BigInteger.valueOf(1);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(text)))) < 0) {
            if (i_1.add(java.math.BigInteger.valueOf(1)).compareTo(new java.math.BigInteger(String.valueOf(_runeLen(text)))) < 0 && (text.substring((int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1)).longValue())+1).equals(text.substring((int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue())+1)))) {
                count_1 = new java.math.BigInteger(String.valueOf(count_1.add(java.math.BigInteger.valueOf(1))));
            } else {
                encoded_1 = encoded_1 + text.substring((int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1)).longValue())+1) + _p(count_1);
                count_1 = java.math.BigInteger.valueOf(1);
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return encoded_1;
    }

    static String run_length_decode(String encoded) {
        String res = "";
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(encoded)))) < 0) {
            String ch_1 = encoded.substring((int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3)).longValue())+1);
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
            String num_str_1 = "";
            while (i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(encoded)))) < 0 && (encoded.substring((int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3)).longValue())+1).compareTo("0") >= 0) && (encoded.substring((int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3)).longValue())+1).compareTo("9") <= 0)) {
                num_str_1 = num_str_1 + encoded.substring((int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3)).longValue())+1);
                i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
            }
            java.math.BigInteger count_3 = new java.math.BigInteger(String.valueOf(Integer.parseInt(num_str_1)));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(count_3) < 0) {
                res = res + ch_1;
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            encoded1 = String.valueOf(run_length_encode(example1));
            System.out.println(encoded1);
            System.out.println(run_length_decode(encoded1));
            encoded2 = String.valueOf(run_length_encode(example2));
            System.out.println(encoded2);
            System.out.println(run_length_decode(encoded2));
            encoded3 = String.valueOf(run_length_encode(example3));
            System.out.println(encoded3);
            System.out.println(run_length_decode(encoded3));
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
