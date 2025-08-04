public class Main {

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (String[] pair : new String[][]{new String[]{"d41d8cd98f00b204e9800998ecf8427e", ""}, new String[]{"0cc175b9c0f1b6a831c399e269772661", "a"}, new String[]{"900150983cd24fb0d6963f7d28e17f72", "abc"}, new String[]{"f96b697d7cb7938d525a2f31aaf161d0", "message digest"}, new String[]{"c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz"}, new String[]{"d174ab98d277d9f5a5611c2c9f419d9f", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"}, new String[]{"57edf4a22be3c955ac49da2e2107b67a", "12345678901234567890" + "123456789012345678901234567890123456789012345678901234567890"}, new String[]{"e38ca1d920c4b8b8d3946b2c72f01680", "The quick brown fox jumped over the lazy dog's back"}}) {
                String sum = String.valueOf(_md5hex(pair[1]));
                if (!(sum.equals(pair[0]))) {
                    System.out.println("MD5 fail");
                    System.out.println("  for string," + " " + pair[1]);
                    System.out.println("  expected:  " + " " + pair[0]);
                    System.out.println("  got:       " + " " + sum);
                }
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

    static String _md5hex(String s) {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("MD5");
            byte[] hash = md.digest(s.getBytes(java.nio.charset.StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (byte b : hash) sb.append(String.format("%02x", b & 0xff));
            return sb.toString();
        } catch (Exception e) { return ""; }
    }
}
