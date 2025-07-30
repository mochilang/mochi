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
        java.math.BigInteger result = java.math.BigInteger.valueOf(1);
        java.math.BigInteger b = base;
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = result.multiply(b);
            }
            b = b.multiply(b);
            e = ((Number)((e / 2))).intValue();
        }
        return result;
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
        java.math.BigInteger d = java.math.BigInteger.valueOf(2);
        while (m.remainder(d).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            factors = java.util.stream.Stream.concat(java.util.Arrays.stream(factors), java.util.stream.Stream.of(d)).toArray(java.math.BigInteger[]::new);
            m = m.divide(d);
        }
        d = java.math.BigInteger.valueOf(3);
        while (d.multiply(d).compareTo(m) <= 0) {
            while (m.remainder(d).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                factors = java.util.stream.Stream.concat(java.util.Arrays.stream(factors), java.util.stream.Stream.of(d)).toArray(java.math.BigInteger[]::new);
                m = m.divide(d);
            }
            d = d.add(java.math.BigInteger.valueOf(2));
        }
        if (m.compareTo(java.math.BigInteger.valueOf(1)) > 0) {
            factors = java.util.stream.Stream.concat(java.util.Arrays.stream(factors), java.util.stream.Stream.of(m)).toArray(java.math.BigInteger[]::new);
        }
        return factors;
    }

    static String show_list(java.math.BigInteger[] xs) {
        String line = "";
        int i = 0;
        while (i < xs.length) {
            line = line + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                line = line + " ";
            }
            i = i + 1;
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
            System.out.println(String.valueOf(n));
        }
        java.util.Map<Integer,java.math.BigInteger[]> extra = ((java.util.Map<Integer,java.math.BigInteger[]>)(new java.util.LinkedHashMap<Integer, java.math.BigInteger[]>(java.util.Map.ofEntries(java.util.Map.entry(6, new java.math.BigInteger[]{java.math.BigInteger.valueOf(274177), java.math.BigInteger.valueOf((int)67280421310721L)}), java.util.Map.entry(7, new java.math.BigInteger[]{parseBigInt("59649589127497217"), parseBigInt("5704689200685129054721")})))));
        System.out.println("\nFactors:");
        int i = 0;
        while (i < nums.length) {
            java.math.BigInteger[] facs = new java.math.BigInteger[]{};
            if (i <= 5) {
                facs = primeFactorsBig(nums[i]);
            } else {
                facs = (java.math.BigInteger[])(((java.math.BigInteger[])(extra).get(i)));
            }
            System.out.println("F" + String.valueOf(i) + " = " + String.valueOf(show_list(facs)));
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
