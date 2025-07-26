public class Main {

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int[] set58(String addr) {
        String tmpl = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
        int[] a = new int[]{};
        int i = 0;
        while (i < 25) {
            a = java.util.stream.IntStream.concat(java.util.Arrays.stream(a), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        int idx = 0;
        while (idx < addr.length()) {
            String ch = addr.substring(idx, idx + 1);
            int c = indexOf(tmpl, ch);
            if (c < 0) {
                return new int[]{};
            }
            int j = 24;
            while (j >= 0) {
                c = c + 58 * a[j];
a[j] = ((Number)(Math.floorMod(c, 256))).intValue();
                c = ((Number)((c / 256))).intValue();
                j = j - 1;
            }
            if (c > 0) {
                return new int[]{};
            }
            idx = idx + 1;
        }
        return a;
    }

    static int[] doubleSHA256(int[] bs) {
        int[] first = _sha256(bs);
        return _sha256(first);
    }

    static int[] computeChecksum(int[] a) {
        int[] hash = doubleSHA256(java.util.Arrays.copyOfRange(a, 0, 21));
        return java.util.Arrays.copyOfRange(hash, 0, 4);
    }

    static boolean validA58(String addr) {
        int[] a = set58(addr);
        if (a.length != 25) {
            return false;
        }
        if (a[0] != 0) {
            return false;
        }
        int[] sum = computeChecksum(a);
        int i = 0;
        while (i < 4) {
            if (a[21 + i] != sum[i]) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(String.valueOf(validA58("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i")));
            System.out.println(String.valueOf(validA58("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j")));
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
