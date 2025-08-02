public class Main {

    static int pow_int(int base, int exp) {
        int result = 1;
        int b = base;
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = result * b;
            }
            b = b * b;
            e = ((Number)((e / 2))).intValue();
        }
        return result;
    }

    static java.math.BigInteger pow_big(java.math.BigInteger base, int exp) {
        java.math.BigInteger result_1 = java.math.BigInteger.valueOf(1);
        java.math.BigInteger b_1 = base;
        int e_1 = exp;
        while (e_1 > 0) {
            if (Math.floorMod(e_1, 2) == 1) {
                result_1 = result_1.multiply(b_1);
            }
            b_1 = b_1.multiply(b_1);
            e_1 = ((Number)((e_1 / 2))).intValue();
        }
        return result_1;
    }

    static java.math.BigInteger parseBigInt(String str) {
        int i = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (_substr(str, 0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        java.math.BigInteger n = java.math.BigInteger.valueOf(0);
        while (i < _runeLen(str)) {
            String ch = _substr(str, i, i + 1);
            int d = Integer.parseInt(ch);
            n = n.multiply((java.math.BigInteger.valueOf(10))).add((new java.math.BigInteger(String.valueOf(d))));
            i = i + 1;
        }
        if (neg) {
            n = (n).negate();
        }
        return n;
    }

    static java.math.BigInteger fermat(int n) {
        int p = pow_int(2, n);
        return pow_big(java.math.BigInteger.valueOf(2), p).add((java.math.BigInteger.valueOf(1)));
    }

    static java.math.BigInteger[] primeFactorsBig(java.math.BigInteger n) {
        java.math.BigInteger[] factors = new java.math.BigInteger[]{};
        java.math.BigInteger m = n;
        java.math.BigInteger d_1 = java.math.BigInteger.valueOf(2);
        while (m.remainder(d_1).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            factors = java.util.stream.Stream.concat(java.util.Arrays.stream(factors), java.util.stream.Stream.of(d_1)).toArray(java.math.BigInteger[]::new);
            m = m.divide(d_1);
        }
        d_1 = java.math.BigInteger.valueOf(3);
        while (d_1.multiply(d_1).compareTo(m) <= 0) {
            while (m.remainder(d_1).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                factors = java.util.stream.Stream.concat(java.util.Arrays.stream(factors), java.util.stream.Stream.of(d_1)).toArray(java.math.BigInteger[]::new);
                m = m.divide(d_1);
            }
            d_1 = d_1.add(java.math.BigInteger.valueOf(2));
        }
        if (m.compareTo(java.math.BigInteger.valueOf(1)) > 0) {
            factors = java.util.stream.Stream.concat(java.util.Arrays.stream(factors), java.util.stream.Stream.of(m)).toArray(java.math.BigInteger[]::new);
        }
        return factors;
    }

    static String show_list(java.math.BigInteger[] xs) {
        String line = "";
        int i_1 = 0;
        while (i_1 < xs.length) {
            line = line + _p(_geto(xs, i_1));
            if (i_1 < xs.length - 1) {
                line = line + " ";
            }
            i_1 = i_1 + 1;
        }
        return line;
    }

    static void main() {
        java.math.BigInteger[] nums = new java.math.BigInteger[]{};
        for (int i = 0; i < 8; i++) {
            nums = java.util.stream.Stream.concat(java.util.Arrays.stream(nums), java.util.stream.Stream.of(fermat(i))).toArray(java.math.BigInteger[]::new);
        }
        System.out.println("First 8 Fermat numbers:");
        for (java.math.BigInteger n : nums) {
            System.out.println(_p(n));
        }
        java.util.Map<Integer,java.math.BigInteger[]> extra = ((java.util.Map<Integer,java.math.BigInteger[]>)(new java.util.LinkedHashMap<Integer, java.math.BigInteger[]>(java.util.Map.ofEntries(java.util.Map.entry(6, new java.math.BigInteger[]{java.math.BigInteger.valueOf(274177), java.math.BigInteger.valueOf((int)67280421310721L)}), java.util.Map.entry(7, new java.math.BigInteger[]{parseBigInt("59649589127497217"), parseBigInt("5704689200685129054721")})))));
        System.out.println("\nFactors:");
        int i_2 = 0;
        while (i_2 < nums.length) {
            java.math.BigInteger[] facs = new java.math.BigInteger[]{};
            if (i_2 <= 5) {
                facs = primeFactorsBig(nums[i_2]);
            } else {
                facs = (java.math.BigInteger[])(((java.math.BigInteger[])(extra).get(i_2)));
            }
            System.out.println("F" + _p(i_2) + " = " + String.valueOf(show_list(facs)));
            i_2 = i_2 + 1;
        }
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
