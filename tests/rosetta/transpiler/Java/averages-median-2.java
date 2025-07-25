public class Main {

    static double sel(double[] list, int k) {
        int i = 0;
        while (i <= k) {
            int minIndex = i;
            int j = i + 1;
            while (j < list.length) {
                if (list[j] < list[minIndex]) {
                    minIndex = j;
                }
                j = j + 1;
            }
            double tmp = list[i];
list[i] = list[minIndex];
list[minIndex] = tmp;
            i = i + 1;
        }
        return list[k];
    }

    static double median(double[] a) {
        double[] arr = a;
        int half = ((Number)((arr.length / 2))).intValue();
        double med = sel(arr, half);
        if (((Number)(Math.floorMod(arr.length, 2))).intValue() == 0) {
            return (med + arr[half - 1]) / 2.0;
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
