public class Main {
    static int[] list;

    static int[] bubbleSort(int[] a) {
        int[] arr = ((int[])(a));
        int itemCount = arr.length - 1;
        while (true) {
            boolean hasChanged = false;
            int index = 0;
            while (index < itemCount) {
                if (arr[index] > arr[index + 1]) {
                    int tmp = arr[index];
arr[index] = arr[index + 1];
arr[index + 1] = tmp;
                    hasChanged = true;
                }
                index = index + 1;
            }
            if (!hasChanged) {
                break;
            }
            itemCount = itemCount - 1;
        }
        return arr;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            list = ((int[])(new int[]{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}));
            System.out.println("unsorted: " + _p(list));
            list = ((int[])(bubbleSort(((int[])(list)))));
            System.out.println("sorted!  " + _p(list));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
