public class Main {

    static int[] quick_sort(int[] items) {
        if (items.length < 2) {
            return items;
        }
        int pivot = items[0];
        int[] lesser = ((int[])(new int[]{}));
        int[] greater = ((int[])(new int[]{}));
        int i = 1;
        while (i < items.length) {
            int item = items[i];
            if (item <= pivot) {
                lesser = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lesser), java.util.stream.IntStream.of(item)).toArray()));
            } else {
                greater = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(greater), java.util.stream.IntStream.of(item)).toArray()));
            }
            i = i + 1;
        }
        return concat(concat(quick_sort(((int[])(lesser))), new int[]{pivot}), quick_sort(((int[])(greater))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("sorted1:" + " " + String.valueOf(quick_sort(((int[])(new int[]{0, 5, 3, 2, 2})))));
            System.out.println("sorted2:" + " " + String.valueOf(quick_sort(((int[])(new int[]{})))));
            System.out.println("sorted3:" + " " + String.valueOf(quick_sort(((int[])(new int[]{-2, 5, 0, -45})))));
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
    }
}
