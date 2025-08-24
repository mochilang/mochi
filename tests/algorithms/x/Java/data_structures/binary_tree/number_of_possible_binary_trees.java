public class Main {
    static String input_str;
    static java.math.BigInteger node_count;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static java.math.BigInteger binomial_coefficient(java.math.BigInteger n, java.math.BigInteger k) {
        java.math.BigInteger result = java.math.BigInteger.valueOf(1);
        java.math.BigInteger kk_1 = new java.math.BigInteger(String.valueOf(k));
        if (k.compareTo(n.subtract(k)) > 0) {
            kk_1 = new java.math.BigInteger(String.valueOf(n.subtract(k)));
        }
        for (int i = 0; i < kk_1; i++) {
            result = new java.math.BigInteger(String.valueOf(result.multiply((n.subtract(new java.math.BigInteger(String.valueOf(i)))))));
            result = new java.math.BigInteger(String.valueOf(result.divide((new java.math.BigInteger(String.valueOf(i)).add(java.math.BigInteger.valueOf(1))))));
        }
        return result;
    }

    static java.math.BigInteger catalan_number(java.math.BigInteger node_count) {
        return binomial_coefficient(new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(2).multiply(node_count))), new java.math.BigInteger(String.valueOf(node_count))).divide((node_count.add(java.math.BigInteger.valueOf(1))));
    }

    static java.math.BigInteger factorial(java.math.BigInteger n) {
        if (n.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            System.out.println("factorial() not defined for negative values");
            return 0;
        }
        java.math.BigInteger result_2 = java.math.BigInteger.valueOf(1);
        for (int i = 1; i < (n.add(java.math.BigInteger.valueOf(1))); i++) {
            result_2 = new java.math.BigInteger(String.valueOf(result_2.multiply(new java.math.BigInteger(String.valueOf(i)))));
        }
        return result_2;
    }

    static java.math.BigInteger binary_tree_count(java.math.BigInteger node_count) {
        return catalan_number(new java.math.BigInteger(String.valueOf(node_count))).multiply(factorial(new java.math.BigInteger(String.valueOf(node_count))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("Enter the number of nodes:");
            input_str = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            node_count = new java.math.BigInteger(String.valueOf(Integer.parseInt(input_str)));
            if (node_count.compareTo(java.math.BigInteger.valueOf(0)) <= 0) {
                System.out.println("We need some nodes to work with.");
            } else {
                java.math.BigInteger bst = new java.math.BigInteger(String.valueOf(catalan_number(new java.math.BigInteger(String.valueOf(node_count)))));
                java.math.BigInteger bt = new java.math.BigInteger(String.valueOf(binary_tree_count(new java.math.BigInteger(String.valueOf(node_count)))));
                System.out.println("Given" + " " + String.valueOf(node_count) + " " + "nodes, there are" + " " + String.valueOf(bt) + " " + "binary trees and" + " " + String.valueOf(bst) + " " + "binary search trees.");
            }
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
