public class Main {
    static String HEX;

    static String byte_to_hex(long b) {
        long hi = Math.floorDiv(b, 16);
        long lo_1 = Math.floorMod(b, 16);
        return HEX.substring((int)((long)(hi)), (int)((long)(hi))+1) + HEX.substring((int)((long)(lo_1)), (int)((long)(lo_1))+1);
    }

    static String bytes_to_hex(long[] bs) {
        String res = "";
        long i_1 = 0L;
        while (i_1 < bs.length) {
            res = res + String.valueOf(byte_to_hex(bs[(int)((long)(i_1))]));
            i_1 = i_1 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        HEX = "0123456789abcdef";
        System.out.println(bytes_to_hex(((long[])(_sha256("Python")))));
        System.out.println(bytes_to_hex(((long[])(_sha256("hello world")))));
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
}
