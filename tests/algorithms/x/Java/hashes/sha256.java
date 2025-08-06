public class Main {
    static String HEX;

    static String byte_to_hex(int b) {
        int hi = b / 16;
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
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            HEX = "0123456789abcdef";
            System.out.println(bytes_to_hex(((int[])(_sha256("Python")))));
            System.out.println(bytes_to_hex(((int[])(_sha256("hello world")))));
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
}
