public class Main {
    static String HEX;
    static String expected;
    static String answer;
    static String computed;

    static String byte_to_hex(int b) {
        int hi = Math.floorDiv(b, 16);
        int lo = Math.floorMod(b, 16);
        return HEX.substring(hi, hi+1) + HEX.substring(lo, lo+1);
    }

    static String bytes_to_hex(int[] bs) {
        String res = "";
        int i = 0;
        while (i < bs.length) {
            res = res + String.valueOf(byte_to_hex(bs[i]));
            i = i + 1;
        }
        return res;
    }

    static String sha256_hex(String s) {
        return bytes_to_hex(((int[])(_sha256(s))));
    }

    static String solution_001() {
        int total = 0;
        int n = 0;
        while (n < 1000) {
            if (Math.floorMod(n, 3) == 0 || Math.floorMod(n, 5) == 0) {
                total = total + n;
            }
            n = n + 1;
        }
        return _p(total);
    }
    public static void main(String[] args) {
        HEX = "0123456789abcdef";
        expected = String.valueOf(sha256_hex("233168"));
        answer = String.valueOf(solution_001());
        computed = String.valueOf(sha256_hex(answer));
        if ((computed.equals(expected))) {
            System.out.println("Problem 001 passed");
        } else {
            System.out.println("Problem 001 failed: " + computed + " != " + expected);
        }
    }

    static int[] _sha256(int[] bs) {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("SHA-256");
            byte[] bytes = new byte[bs.length];
            for (int i = 0; i < bs.length; i++) bytes[i] = (byte)bs[i];
            byte[] hash = md.digest(bytes);
            int[] out = new int[hash.length];
            for (int i = 0; i < hash.length; i++) out[i] = hash[i] & 0xff;
            return out;
        } catch (Exception e) { return new int[0]; }
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
