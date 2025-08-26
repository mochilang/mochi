public class Main {
    static java.math.BigInteger[] primes_2;
    static java.math.BigInteger t;
    static java.math.BigInteger case_idx = java.math.BigInteger.valueOf(0);

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static String[] split(String s, String sep) {
        String[] parts = ((String[])(new String[]{}));
        String cur_1 = "";
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            if (new java.math.BigInteger(String.valueOf(_runeLen(sep))).compareTo(java.math.BigInteger.valueOf(0)) > 0 && i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep)))).compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) <= 0 && (_substr(s, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep)))))).longValue())).equals(sep))) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
                cur_1 = "";
                i_1 = i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep))));
            } else {
                cur_1 = cur_1 + _substr(s, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue()));
                i_1 = i_1.add(java.math.BigInteger.valueOf(1));
            }
        }
        parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
        return ((String[])(parts));
    }

    static java.math.BigInteger[] precompute(java.math.BigInteger limit) {
        boolean[] sieve = ((boolean[])(new boolean[]{}));
        for (java.math.BigInteger i = java.math.BigInteger.valueOf(0); i.compareTo((limit.add(java.math.BigInteger.valueOf(1)))) < 0; i = i.add(java.math.BigInteger.ONE)) {
            sieve = ((boolean[])(appendBool(sieve, true)));
        }
sieve[(int)(0L)] = false;
sieve[(int)(1L)] = false;
        java.math.BigInteger p_1 = java.math.BigInteger.valueOf(2);
        while (p_1.multiply(p_1).compareTo(limit) <= 0) {
            if (sieve[_idx((sieve).length, ((java.math.BigInteger)(p_1)).longValue())]) {
                java.math.BigInteger j_1 = p_1.multiply(p_1);
                while (j_1.compareTo(limit) <= 0) {
sieve[(int)(((java.math.BigInteger)(j_1)).longValue())] = false;
                    j_1 = j_1.add(p_1);
                }
            }
            p_1 = p_1.add(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger[] primes_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        for (java.math.BigInteger i = java.math.BigInteger.valueOf(2); i.compareTo((limit.add(java.math.BigInteger.valueOf(1)))) < 0; i = i.add(java.math.BigInteger.ONE)) {
            if (sieve[_idx((sieve).length, ((java.math.BigInteger)(i)).longValue())]) {
                primes_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(primes_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(i)))).toArray(java.math.BigInteger[]::new)));
            }
        }
        return ((java.math.BigInteger[])(primes_1));
    }
    public static void main(String[] args) {
        primes_2 = ((java.math.BigInteger[])(precompute(java.math.BigInteger.valueOf(32000))));
        t = new java.math.BigInteger(String.valueOf(Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""))));
        while (case_idx.compareTo(t) < 0) {
            String line = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            String[] parts_1 = ((String[])(line.split(java.util.regex.Pattern.quote(" "))));
            java.math.BigInteger m = new java.math.BigInteger(String.valueOf(Integer.parseInt(parts_1[_idx((parts_1).length, 0L)])));
            java.math.BigInteger n = new java.math.BigInteger(String.valueOf(Integer.parseInt(parts_1[_idx((parts_1).length, 1L)])));
            java.math.BigInteger size = n.subtract(m).add(java.math.BigInteger.valueOf(1));
            boolean[] segment = ((boolean[])(new boolean[]{}));
            for (java.math.BigInteger i = java.math.BigInteger.valueOf(0); i.compareTo(size) < 0; i = i.add(java.math.BigInteger.ONE)) {
                segment = ((boolean[])(appendBool(segment, true)));
            }
            for (java.math.BigInteger p : primes_2) {
                if (p.multiply(p).compareTo(n) > 0) {
                    break;
                }
                java.math.BigInteger start = p.multiply(p);
                if (start.compareTo(m) < 0) {
                    java.math.BigInteger rem = m.remainder(p);
                    if (rem.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                        start = m;
                    } else {
                        start = m.add((p.subtract(rem)));
                    }
                }
                java.math.BigInteger j_2 = start;
                while (j_2.compareTo(n) <= 0) {
segment[(int)(((java.math.BigInteger)(j_2.subtract(m))).longValue())] = false;
                    j_2 = j_2.add(p);
                }
            }
            if (m.compareTo(java.math.BigInteger.valueOf(1)) == 0) {
segment[(int)(0L)] = false;
            }
            java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
            while (i_2.compareTo(size) < 0) {
                if (segment[_idx((segment).length, ((java.math.BigInteger)(i_2)).longValue())]) {
                    System.out.println(i_2.add(m));
                }
                i_2 = i_2.add(java.math.BigInteger.valueOf(1));
            }
            if (case_idx.compareTo(t.subtract(java.math.BigInteger.valueOf(1))) < 0) {
                System.out.println("");
            }
            case_idx = case_idx.add(java.math.BigInteger.valueOf(1));
        }
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
