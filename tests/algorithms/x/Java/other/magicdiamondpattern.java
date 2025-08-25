public class Main {

    static String floyd(java.math.BigInteger n) {
        String result = "";
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(n.subtract(i_1).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                result = result + " ";
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
            while (k_1.compareTo(i_1.add(java.math.BigInteger.valueOf(1))) < 0) {
                result = result + "* ";
                k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
            }
            result = result + "\n";
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return result;
    }

    static String reverse_floyd(java.math.BigInteger n) {
        String result_1 = "";
        java.math.BigInteger i_3 = new java.math.BigInteger(String.valueOf(n));
        while (i_3.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger j_3 = new java.math.BigInteger(String.valueOf(i_3));
            while (j_3.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                result_1 = result_1 + "* ";
                j_3 = new java.math.BigInteger(String.valueOf(j_3.subtract(java.math.BigInteger.valueOf(1))));
            }
            result_1 = result_1 + "\n";
            java.math.BigInteger k_3 = new java.math.BigInteger(String.valueOf(n.subtract(i_3).add(java.math.BigInteger.valueOf(1))));
            while (k_3.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                result_1 = result_1 + " ";
                k_3 = new java.math.BigInteger(String.valueOf(k_3.subtract(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.subtract(java.math.BigInteger.valueOf(1))));
        }
        return result_1;
    }

    static String pretty_print(java.math.BigInteger n) {
        if (n.compareTo(java.math.BigInteger.valueOf(0)) <= 0) {
            return "       ...       ....        nothing printing :(";
        }
        String upper_half_1 = String.valueOf(floyd(new java.math.BigInteger(String.valueOf(n))));
        String lower_half_1 = String.valueOf(reverse_floyd(new java.math.BigInteger(String.valueOf(n))));
        return upper_half_1 + lower_half_1;
    }

    static void main() {
        System.out.println(pretty_print(java.math.BigInteger.valueOf(3)));
        System.out.println(pretty_print(java.math.BigInteger.valueOf(0)));
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
}
