public class Main {

    static int toUnsigned16(int n) {
        int u = n;
        if (u < 0) {
            u = u + 65536;
        }
        return Math.floorMod(u, 65536);
    }

    static String bin16(int n) {
        int u_1 = toUnsigned16(n);
        String bits = "";
        int mask = 32768;
        for (int i = 0; i < 16; i++) {
            if (u_1 >= mask) {
                bits = bits + "1";
                u_1 = u_1 - mask;
            } else {
                bits = bits + "0";
            }
            mask = ((Number)((mask / 2))).intValue();
        }
        return bits;
    }

    static int bit_and(int a, int b) {
        int ua = toUnsigned16(a);
        int ub = toUnsigned16(b);
        int res = 0;
        int bit = 1;
        for (int i = 0; i < 16; i++) {
            if (Math.floorMod(ua, 2) == 1 && Math.floorMod(ub, 2) == 1) {
                res = res + bit;
            }
            ua = ((Number)((ua / 2))).intValue();
            ub = ((Number)((ub / 2))).intValue();
            bit = bit * 2;
        }
        return res;
    }

    static int bit_or(int a, int b) {
        int ua_1 = toUnsigned16(a);
        int ub_1 = toUnsigned16(b);
        int res_1 = 0;
        int bit_1 = 1;
        for (int i = 0; i < 16; i++) {
            if (Math.floorMod(ua_1, 2) == 1 || Math.floorMod(ub_1, 2) == 1) {
                res_1 = res_1 + bit_1;
            }
            ua_1 = ((Number)((ua_1 / 2))).intValue();
            ub_1 = ((Number)((ub_1 / 2))).intValue();
            bit_1 = bit_1 * 2;
        }
        return res_1;
    }

    static int bit_xor(int a, int b) {
        int ua_2 = toUnsigned16(a);
        int ub_2 = toUnsigned16(b);
        int res_2 = 0;
        int bit_2 = 1;
        for (int i = 0; i < 16; i++) {
            int abit = Math.floorMod(ua_2, 2);
            int bbit = Math.floorMod(ub_2, 2);
            if ((abit == 1 && bbit == 0) || (abit == 0 && bbit == 1)) {
                res_2 = res_2 + bit_2;
            }
            ua_2 = ((Number)((ua_2 / 2))).intValue();
            ub_2 = ((Number)((ub_2 / 2))).intValue();
            bit_2 = bit_2 * 2;
        }
        return res_2;
    }

    static int bit_not(int a) {
        int ua_3 = toUnsigned16(a);
        return 65535 - ua_3;
    }

    static int shl(int a, int b) {
        int ua_4 = toUnsigned16(a);
        int i = 0;
        while (i < b) {
            ua_4 = Math.floorMod((ua_4 * 2), 65536);
            i = i + 1;
        }
        return ua_4;
    }

    static int shr(int a, int b) {
        int ua_5 = toUnsigned16(a);
        int i_1 = 0;
        while (i_1 < b) {
            ua_5 = ((Number)((ua_5 / 2))).intValue();
            i_1 = i_1 + 1;
        }
        return ua_5;
    }

    static int las(int a, int b) {
        return shl(a, b);
    }

    static int ras(int a, int b) {
        int val = a;
        int i_2 = 0;
        while (i_2 < b) {
            if (val >= 0) {
                val = ((Number)((val / 2))).intValue();
            } else {
                val = ((Number)(((val - 1) / 2))).intValue();
            }
            i_2 = i_2 + 1;
        }
        return toUnsigned16(val);
    }

    static int rol(int a, int b) {
        int ua_6 = toUnsigned16(a);
        int left = shl(ua_6, b);
        int right = shr(ua_6, 16 - b);
        return toUnsigned16(left + right);
    }

    static int ror(int a, int b) {
        int ua_7 = toUnsigned16(a);
        int right_1 = shr(ua_7, b);
        int left_1 = shl(ua_7, 16 - b);
        return toUnsigned16(left_1 + right_1);
    }

    static void bitwise(int a, int b) {
        System.out.println("a:   " + String.valueOf(bin16(a)));
        System.out.println("b:   " + String.valueOf(bin16(b)));
        System.out.println("and: " + String.valueOf(bin16(bit_and(a, b))));
        System.out.println("or:  " + String.valueOf(bin16(bit_or(a, b))));
        System.out.println("xor: " + String.valueOf(bin16(bit_xor(a, b))));
        System.out.println("not: " + String.valueOf(bin16(bit_not(a))));
        if (b < 0) {
            System.out.println("Right operand is negative, but all shifts require an unsigned right operand (shift distance).");
            return;
        }
        System.out.println("shl: " + String.valueOf(bin16(shl(a, b))));
        System.out.println("shr: " + String.valueOf(bin16(shr(a, b))));
        System.out.println("las: " + String.valueOf(bin16(las(a, b))));
        System.out.println("ras: " + String.valueOf(bin16(ras(a, b))));
        System.out.println("rol: " + String.valueOf(bin16(rol(a, b))));
        System.out.println("ror: " + String.valueOf(bin16(ror(a, b))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            bitwise(-460, 6);
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
}
