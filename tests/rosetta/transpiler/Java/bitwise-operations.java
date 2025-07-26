public class Main {

    static int toUnsigned16(int n) {
        int u = n;
        if (u < 0) {
            u = u + 65536;
        }
        return Math.floorMod(u, 65536);
    }

    static String bin16(int n) {
        int u = toUnsigned16(n);
        String bits = "";
        int mask = 32768;
        for (int i = 0; i < 16; i++) {
            if (u >= mask) {
                bits = bits + "1";
                u = u - mask;
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
        int ua = toUnsigned16(a);
        int ub = toUnsigned16(b);
        int res = 0;
        int bit = 1;
        for (int i = 0; i < 16; i++) {
            if (Math.floorMod(ua, 2) == 1 || Math.floorMod(ub, 2) == 1) {
                res = res + bit;
            }
            ua = ((Number)((ua / 2))).intValue();
            ub = ((Number)((ub / 2))).intValue();
            bit = bit * 2;
        }
        return res;
    }

    static int bit_xor(int a, int b) {
        int ua = toUnsigned16(a);
        int ub = toUnsigned16(b);
        int res = 0;
        int bit = 1;
        for (int i = 0; i < 16; i++) {
            int abit = Math.floorMod(ua, 2);
            int bbit = Math.floorMod(ub, 2);
            if ((abit == 1 && bbit == 0) || (abit == 0 && bbit == 1)) {
                res = res + bit;
            }
            ua = ((Number)((ua / 2))).intValue();
            ub = ((Number)((ub / 2))).intValue();
            bit = bit * 2;
        }
        return res;
    }

    static int bit_not(int a) {
        int ua = toUnsigned16(a);
        return 65535 - ua;
    }

    static int shl(int a, int b) {
        int ua = toUnsigned16(a);
        int i = 0;
        while (i < b) {
            ua = Math.floorMod((ua * 2), 65536);
            i = i + 1;
        }
        return ua;
    }

    static int shr(int a, int b) {
        int ua = toUnsigned16(a);
        int i = 0;
        while (i < b) {
            ua = ((Number)((ua / 2))).intValue();
            i = i + 1;
        }
        return ua;
    }

    static int las(int a, int b) {
        return shl(a, b);
    }

    static int ras(int a, int b) {
        int val = a;
        int i = 0;
        while (i < b) {
            if (val >= 0) {
                val = ((Number)((val / 2))).intValue();
            } else {
                val = ((Number)(((val - 1) / 2))).intValue();
            }
            i = i + 1;
        }
        return toUnsigned16(val);
    }

    static int rol(int a, int b) {
        int ua = toUnsigned16(a);
        int left = shl(ua, b);
        int right = shr(ua, 16 - b);
        return toUnsigned16(left + right);
    }

    static int ror(int a, int b) {
        int ua = toUnsigned16(a);
        int right = shr(ua, b);
        int left = shl(ua, 16 - b);
        return toUnsigned16(left + right);
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
        bitwise(-460, 6);
    }
}
