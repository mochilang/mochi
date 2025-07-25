public class Main {

    static double qsel(double[] a, int k) {
        double[] arr = a;
        while (arr.length > 1) {
            int px = Math.floorMod(_now(), arr.length);
            double pv = arr[px];
            int last = arr.length - 1;
            double tmp = arr[px];
arr[px] = arr[last];
arr[last] = tmp;
            px = 0;
            int i = 0;
            while (i < last) {
                double v = arr[i];
                if (v < pv) {
                    double tmp2 = arr[px];
arr[px] = arr[i];
arr[i] = tmp2;
                    px = px + 1;
                }
                i = i + 1;
            }
            if (px == k) {
                return pv;
            }
            if (k < px) {
                arr = java.util.Arrays.copyOfRange(arr, 0, px);
            } else {
                double tmp2 = arr[px];
arr[px] = pv;
arr[last] = tmp2;
                arr = java.util.Arrays.copyOfRange(arr, (px + 1), arr.length);
                k = k - (px + 1);
            }
        }
        return arr[0];
    }

    static double median(double[] list) {
        double[] arr = list;
        int half = ((Number)((arr.length / 2))).intValue();
        double med = qsel(arr, half);
        if (((Number)(Math.floorMod(arr.length, 2))).intValue() == 0) {
            return (med + qsel(arr, half - 1)) / 2.0;
        }
        return med;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(String.valueOf(median(new double[]{3.0, 1.0, 4.0, 1.0})));
            System.out.println(String.valueOf(median(new double[]{3.0, 1.0, 4.0, 1.0, 5.0})));
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
