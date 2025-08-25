public class Main {

    static boolean is_happy_number(java.math.BigInteger num) {
        if (num.compareTo(java.math.BigInteger.valueOf(0)) <= 0) {
            throw new RuntimeException(String.valueOf("num must be a positive integer"));
        }
        java.math.BigInteger[] seen_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(num));
        while (n_1.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
            while (i_1.compareTo(new java.math.BigInteger(String.valueOf(seen_1.length))) < 0) {
                if (seen_1[_idx((seen_1).length, ((java.math.BigInteger)(i_1)).longValue())].compareTo(n_1) == 0) {
                    return false;
                }
                i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
            }
            seen_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(seen_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(n_1)))).toArray(java.math.BigInteger[]::new)));
            java.math.BigInteger total_1 = java.math.BigInteger.valueOf(0);
            java.math.BigInteger temp_1 = new java.math.BigInteger(String.valueOf(n_1));
            while (temp_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                java.math.BigInteger digit_1 = new java.math.BigInteger(String.valueOf(temp_1.remainder(java.math.BigInteger.valueOf(10))));
                total_1 = new java.math.BigInteger(String.valueOf(total_1.add(digit_1.multiply(digit_1))));
                temp_1 = new java.math.BigInteger(String.valueOf(temp_1.divide(java.math.BigInteger.valueOf(10))));
            }
            n_1 = new java.math.BigInteger(String.valueOf(total_1));
        }
        return true;
    }

    static void test_is_happy_number() {
        if (!(Boolean)is_happy_number(java.math.BigInteger.valueOf(19))) {
            throw new RuntimeException(String.valueOf("19 should be happy"));
        }
        if (is_happy_number(java.math.BigInteger.valueOf(2))) {
            throw new RuntimeException(String.valueOf("2 should be unhappy"));
        }
        if (!(Boolean)is_happy_number(java.math.BigInteger.valueOf(23))) {
            throw new RuntimeException(String.valueOf("23 should be happy"));
        }
        if (!(Boolean)is_happy_number(java.math.BigInteger.valueOf(1))) {
            throw new RuntimeException(String.valueOf("1 should be happy"));
        }
    }

    static void main() {
        test_is_happy_number();
        System.out.println(is_happy_number(java.math.BigInteger.valueOf(19)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
