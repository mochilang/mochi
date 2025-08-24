public class Main {
    static java.math.BigInteger[] arr1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(5)}));
    static java.math.BigInteger[] arr2 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(5)}));

    static java.math.BigInteger partition(java.math.BigInteger[] arr, java.math.BigInteger low, java.math.BigInteger high) {
        java.math.BigInteger pivot = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(high)).longValue())]));
        java.math.BigInteger i_1 = new java.math.BigInteger(String.valueOf(low.subtract(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(low));
        while (j_1.compareTo(high) < 0) {
            if (arr[(int)(((java.math.BigInteger)(j_1)).longValue())].compareTo(pivot) >= 0) {
                i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
                java.math.BigInteger tmp_1 = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(i_1)).longValue())]));
arr[(int)(((java.math.BigInteger)(i_1)).longValue())] = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(j_1)).longValue())]));
arr[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf(tmp_1));
            }
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger k_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger tmp_3 = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(k_1)).longValue())]));
arr[(int)(((java.math.BigInteger)(k_1)).longValue())] = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(high)).longValue())]));
arr[(int)(((java.math.BigInteger)(high)).longValue())] = new java.math.BigInteger(String.valueOf(tmp_3));
        return k_1;
    }

    static java.math.BigInteger kth_largest_element(java.math.BigInteger[] arr, java.math.BigInteger position) {
        if (new java.math.BigInteger(String.valueOf(arr.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return (java.math.BigInteger.valueOf(1)).negate();
        }
        if (position.compareTo(java.math.BigInteger.valueOf(1)) < 0 || position.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) > 0) {
            return (java.math.BigInteger.valueOf(1)).negate();
        }
        java.math.BigInteger low_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger high_1 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(arr.length)).subtract(java.math.BigInteger.valueOf(1))));
        while (low_1.compareTo(high_1) <= 0) {
            if (low_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length)).subtract(java.math.BigInteger.valueOf(1))) > 0 || high_1.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
                return (java.math.BigInteger.valueOf(1)).negate();
            }
            java.math.BigInteger pivot_index_1 = new java.math.BigInteger(String.valueOf(partition(((java.math.BigInteger[])(arr)), new java.math.BigInteger(String.valueOf(low_1)), new java.math.BigInteger(String.valueOf(high_1)))));
            if (pivot_index_1.compareTo(position.subtract(java.math.BigInteger.valueOf(1))) == 0) {
                return arr[(int)(((java.math.BigInteger)(pivot_index_1)).longValue())];
            } else             if (pivot_index_1.compareTo(position.subtract(java.math.BigInteger.valueOf(1))) > 0) {
                high_1 = new java.math.BigInteger(String.valueOf(pivot_index_1.subtract(java.math.BigInteger.valueOf(1))));
            } else {
                low_1 = new java.math.BigInteger(String.valueOf(pivot_index_1.add(java.math.BigInteger.valueOf(1))));
            }
        }
        return (java.math.BigInteger.valueOf(1)).negate();
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(kth_largest_element(((java.math.BigInteger[])(arr1)), java.math.BigInteger.valueOf(3)));
            System.out.println("\n");
            System.out.println(kth_largest_element(((java.math.BigInteger[])(arr2)), java.math.BigInteger.valueOf(1)));
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
