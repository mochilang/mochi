public class Main {
    static String ascii;

    static int ord(String ch) {
        int i = 0;
        while (i < _runeLen(ascii)) {
            if ((ascii.substring(i, i + 1).equals(ch))) {
                return 32 + i;
            }
            i = i + 1;
        }
        return 0;
    }

    static int bit_and(int a, int b) {
        int ua = a;
        int ub = b;
        int res = 0;
        int bit = 1;
        while (ua > 0 || ub > 0) {
            if (Math.floorMod(ua, 2) == 1 && Math.floorMod(ub, 2) == 1) {
                res = res + bit;
            }
            ua = ((Number)((ua / 2))).intValue();
            ub = ((Number)((ub / 2))).intValue();
            bit = bit * 2;
        }
        return res;
    }

    static int bit_xor(int a, int b) {
        int ua_1 = a;
        int ub_1 = b;
        int res_1 = 0;
        int bit_1 = 1;
        while (ua_1 > 0 || ub_1 > 0) {
            int abit = Math.floorMod(ua_1, 2);
            int bbit = Math.floorMod(ub_1, 2);
            if (abit != bbit) {
                res_1 = res_1 + bit_1;
            }
            ua_1 = ((Number)((ua_1 / 2))).intValue();
            ub_1 = ((Number)((ub_1 / 2))).intValue();
            bit_1 = bit_1 * 2;
        }
        return res_1;
    }

    static int bit_not32(int x) {
        int ux = x;
        int res_2 = 0;
        int bit_2 = 1;
        int count = 0;
        while (count < 32) {
            if (Math.floorMod(ux, 2) == 0) {
                res_2 = res_2 + bit_2;
            }
            ux = ((Number)((ux / 2))).intValue();
            bit_2 = bit_2 * 2;
            count = count + 1;
        }
        return res_2;
    }

    static int elf_hash(String data) {
        int hash_ = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(data)) {
            int c = ord(data.substring(i_1, i_1 + 1));
            hash_ = hash_ * 16 + c;
            int x = bit_and(hash_, (int)4026531840L);
            if (x != 0) {
                hash_ = bit_xor(hash_, ((Number)((x / 16777216))).intValue());
            }
            hash_ = bit_and(hash_, bit_not32(x));
            i_1 = i_1 + 1;
        }
        return hash_;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ascii = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
            System.out.println(_p(elf_hash("lorem ipsum")));
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
