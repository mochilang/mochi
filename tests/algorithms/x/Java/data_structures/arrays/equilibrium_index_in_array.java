public class Main {
    static java.math.BigInteger[] arr1 = new java.math.BigInteger[0];
    static java.math.BigInteger[] arr2 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5)}));
    static java.math.BigInteger[] arr3 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1)}));
    static java.math.BigInteger[] arr4 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(3)}));

    static java.math.BigInteger equilibrium_index(java.math.BigInteger[] arr) {
        java.math.BigInteger total = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            total = new java.math.BigInteger(String.valueOf(total.add(arr[(int)(((java.math.BigInteger)(i_1)).longValue())])));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger left_1 = java.math.BigInteger.valueOf(0);
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            total = new java.math.BigInteger(String.valueOf(total.subtract(arr[(int)(((java.math.BigInteger)(i_1)).longValue())])));
            if (left_1.compareTo(total) == 0) {
                return i_1;
            }
            left_1 = new java.math.BigInteger(String.valueOf(left_1.add(arr[(int)(((java.math.BigInteger)(i_1)).longValue())])));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (java.math.BigInteger.valueOf(1)).negate();
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            arr1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(7)).negate())), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(4)).negate())), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0)}));
            System.out.println(equilibrium_index(((java.math.BigInteger[])(arr1))));
            System.out.println(equilibrium_index(((java.math.BigInteger[])(arr2))));
            System.out.println(equilibrium_index(((java.math.BigInteger[])(arr3))));
            System.out.println(equilibrium_index(((java.math.BigInteger[])(arr4))));
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
