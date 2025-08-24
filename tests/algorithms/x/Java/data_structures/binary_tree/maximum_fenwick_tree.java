public class Main {
    static java.math.BigInteger[] arr = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)}));

    static java.math.BigInteger[] zeros(java.math.BigInteger n) {
        java.math.BigInteger[] res = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            res = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return res;
    }

    static void update(java.math.BigInteger[] arr, java.math.BigInteger idx, java.math.BigInteger value) {
arr[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(value));
    }

    static java.math.BigInteger query(java.math.BigInteger[] arr, java.math.BigInteger left, java.math.BigInteger right) {
        java.math.BigInteger result = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_3 = new java.math.BigInteger(String.valueOf(left));
        while (i_3.compareTo(right) < 0) {
            if (arr[(int)(((java.math.BigInteger)(i_3)).longValue())].compareTo(result) > 0) {
                result = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(i_3)).longValue())]));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5)));
            update(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(100));
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5)));
            update(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(0));
            update(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(20));
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5)));
            update(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(10));
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5)));
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5)));
            update(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(0));
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5)));
            arr = ((java.math.BigInteger[])(zeros(java.math.BigInteger.valueOf(10000))));
            update(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(255), java.math.BigInteger.valueOf(30));
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(10000)));
            arr = ((java.math.BigInteger[])(zeros(java.math.BigInteger.valueOf(6))));
            update(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(1));
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6)));
            arr = ((java.math.BigInteger[])(zeros(java.math.BigInteger.valueOf(6))));
            update(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1000));
            System.out.println(query(((java.math.BigInteger[])(arr)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1)));
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
}
