public class Main {
    static int[] table = crc32Table();

    static int xor(int a, int b) {
        int res = 0;
        int bit = 1;
        int x = a;
        int y = b;
        while (x > 0 || y > 0) {
            int abit = Math.floorMod(x, 2);
            int bbit = Math.floorMod(y, 2);
            if (abit != bbit) {
                res = res + bit;
            }
            x = x / 2;
            y = y / 2;
            bit = bit * 2;
        }
        return res;
    }

    static int rshift(int x, int n) {
        int v = x;
        int i = 0;
        while (i < n) {
            v = v / 2;
            i = i + 1;
        }
        return v;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int idx = ((Number)(upper.indexOf(ch))).intValue();
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = ((Number)(lower.indexOf(ch))).intValue();
        if (idx >= 0) {
            return 97 + idx;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        return 0;
    }

    static String toHex(int n) {
        String digits = "0123456789ABCDEF";
        if (n == 0) {
            return "0";
        }
        int v = n;
        String out = "";
        while (v > 0) {
            int d = Math.floorMod(v, 16);
            out = digits.substring(d, d + 1) + out;
            v = v / 16;
        }
        return out;
    }

    static int[] crc32Table() {
        int[] table = new int[]{};
        int i = 0;
        while (i < 256) {
            int word = i;
            int j = 0;
            while (j < 8) {
                if (Math.floorMod(word, 2) == 1) {
                    word = xor(rshift(word, 1), (int)3988292384L);
                } else {
                    word = rshift(word, 1);
                }
                j = j + 1;
            }
            table = java.util.stream.IntStream.concat(java.util.Arrays.stream(table), java.util.stream.IntStream.of(word)).toArray();
            i = i + 1;
        }
        return table;
    }

    static int crc32(String s) {
        int crc = (int)4294967295L;
        int i = 0;
        while (i < _runeLen(s)) {
            int c = ord(s.substring(i, i + 1));
            int idx = xor(Math.floorMod(crc, 256), c);
            crc = xor(table[idx], rshift(crc, 8));
            i = i + 1;
        }
        return (int)4294967295L - crc;
    }

    static void main() {
        String s = "The quick brown fox jumps over the lazy dog";
        int result = crc32(s);
        String hex = String.valueOf(toHex(result));
        System.out.println(hex);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
