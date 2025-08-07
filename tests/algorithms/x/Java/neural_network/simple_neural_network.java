public class Main {
    static int seed = 0;
    static double INITIAL_VALUE;
    static double result;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static int randint(int low, int high) {
        return (Math.floorMod(rand(), (high - low + 1))) + low;
    }

    static double expApprox(double x) {
        double y = x;
        boolean is_neg = false;
        if (x < 0.0) {
            is_neg = true;
            y = -x;
        }
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 30) {
            term = term * y / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        if (((Boolean)(is_neg))) {
            return 1.0 / sum;
        }
        return sum;
    }

    static double sigmoid(double x) {
        return 1.0 / (1.0 + expApprox(-x));
    }

    static double sigmoid_derivative(double sig_val) {
        return sig_val * (1.0 - sig_val);
    }

    static double forward_propagation(int expected, int number_propagations) {
        double weight = 2.0 * (((Number)(randint(1, 100))).doubleValue()) - 1.0;
        double layer_1 = 0.0;
        int i = 0;
        while (i < number_propagations) {
            layer_1 = sigmoid(INITIAL_VALUE * weight);
            double layer_1_error = (((Number)(expected)).doubleValue() / 100.0) - layer_1;
            double layer_1_delta = layer_1_error * sigmoid_derivative(layer_1);
            weight = weight + INITIAL_VALUE * layer_1_delta;
            i = i + 1;
        }
        return layer_1 * 100.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
            INITIAL_VALUE = 0.02;
            seed = 1;
            result = forward_propagation(32, 450000);
            System.out.println(result);
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
}
