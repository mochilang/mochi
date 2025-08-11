public class Main {
    static String HEX;
    static String expected;
    static String answer;
    static String computed;

    static String byte_to_hex(long b) {
        long hi = Math.floorDiv(b, 16);
        long lo_1 = Math.floorMod(b, 16);
        return HEX.substring((int)(hi), (int)(hi)+1) + HEX.substring((int)(lo_1), (int)(lo_1)+1);
    }

    static String bytes_to_hex(long[] bs) {
        String res = "";
        long i_1 = 0;
        while (i_1 < bs.length) {
            res = res + String.valueOf(byte_to_hex(bs[(int)(i_1)]));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String sha256_hex(String s) {
        return bytes_to_hex(((long[])(_sha256(s))));
    }

    static String solution_001() {
        long total = 0;
        long n_1 = 0;
        while (n_1 < 1000) {
            if (Math.floorMod(n_1, 3) == 0 || Math.floorMod(n_1, 5) == 0) {
                total = total + n_1;
            }
            n_1 = n_1 + 1;
        }
        return _p(total);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            HEX = "0123456789abcdef";
            expected = String.valueOf(sha256_hex("233168"));
            answer = String.valueOf(solution_001());
            computed = String.valueOf(sha256_hex(answer));
            if ((computed.equals(expected))) {
                System.out.println("Problem 001 passed");
            } else {
                System.out.println("Problem 001 failed: " + computed + " != " + expected);
            }
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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

    static long[] _sha256(long[] bs) {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("SHA-256");
            byte[] bytes = new byte[bs.length];
            for (int i = 0; i < bs.length; i++) bytes[i] = (byte)bs[i];
            byte[] hash = md.digest(bytes);
            long[] out = new long[hash.length];
            for (int i = 0; i < hash.length; i++) out[i] = hash[i] & 0xff;
            return out;
        } catch (Exception e) { return new long[0]; }
    }

    static long[] _sha256(String s) {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("SHA-256");
            byte[] bytes = s.getBytes(java.nio.charset.StandardCharsets.UTF_8);
            byte[] hash = md.digest(bytes);
            long[] out = new long[hash.length];
            for (int i = 0; i < hash.length; i++) out[i] = hash[i] & 0xff;
            return out;
        } catch (Exception e) { return new long[0]; }
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
