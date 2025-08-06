public class Main {

    static int max_product_subarray(int[] numbers) {
        if (numbers.length == 0) {
            return 0;
        }
        int max_till_now = numbers[0];
        int min_till_now = numbers[0];
        int max_prod = numbers[0];
        int i = 1;
        while (i < numbers.length) {
            int number = numbers[i];
            if (number < 0) {
                int temp = max_till_now;
                max_till_now = min_till_now;
                min_till_now = temp;
            }
            int prod_max = max_till_now * number;
            if (number > prod_max) {
                max_till_now = number;
            } else {
                max_till_now = prod_max;
            }
            int prod_min = min_till_now * number;
            if (number < prod_min) {
                min_till_now = number;
            } else {
                min_till_now = prod_min;
            }
            if (max_till_now > max_prod) {
                max_prod = max_till_now;
            }
            i = i + 1;
        }
        return max_prod;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(max_product_subarray(((int[])(new int[]{2, 3, -2, 4}))));
            System.out.println(max_product_subarray(((int[])(new int[]{-2, 0, -1}))));
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
