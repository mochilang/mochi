public class Main {
    static double[] heap = new double[0];
    static int size_1 = 0;
    static double m;

    static int parent_index(int child_idx) {
        if (child_idx > 0) {
            return ((int)(Math.floorDiv((child_idx - 1), 2)));
        }
        return -1;
    }

    static int left_child_idx(int parent_idx) {
        return 2 * parent_idx + 1;
    }

    static int right_child_idx(int parent_idx) {
        return 2 * parent_idx + 2;
    }

    static void max_heapify(double[] h, int heap_size, int index) {
        int largest = index;
        int left = left_child_idx(index);
        int right = right_child_idx(index);
        if (left < heap_size && h[left] > h[largest]) {
            largest = left;
        }
        if (right < heap_size && h[right] > h[largest]) {
            largest = right;
        }
        if (largest != index) {
            double temp = h[index];
h[index] = h[largest];
h[largest] = temp;
            max_heapify(((double[])(h)), heap_size, largest);
        }
    }

    static int build_max_heap(double[] h) {
        int heap_size = h.length;
        int i = Math.floorDiv(heap_size, 2) - 1;
        while (i >= 0) {
            max_heapify(((double[])(h)), heap_size, i);
            i = i - 1;
        }
        return heap_size;
    }

    static double extract_max(double[] h, int heap_size) {
        double max_value = h[0];
h[0] = h[heap_size - 1];
        max_heapify(((double[])(h)), heap_size - 1, 0);
        return max_value;
    }

    static int insert(double[] h, int heap_size, double value) {
        if (heap_size < h.length) {
h[heap_size] = value;
        } else {
            h = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(h), java.util.stream.DoubleStream.of(value)).toArray()));
        }
        heap_size = heap_size + 1;
        int idx = Math.floorDiv((heap_size - 1), 2);
        while (idx >= 0) {
            max_heapify(((double[])(h)), heap_size, idx);
            idx = Math.floorDiv((idx - 1), 2);
        }
        return heap_size;
    }

    static void heap_sort(double[] h, int heap_size) {
        int size = heap_size;
        int j = size - 1;
        while (j > 0) {
            double temp_1 = h[0];
h[0] = h[j];
h[j] = temp_1;
            size = size - 1;
            max_heapify(((double[])(h)), size, 0);
            j = j - 1;
        }
    }

    static String heap_to_string(double[] h, int heap_size) {
        String s = "[";
        int i_1 = 0;
        while (i_1 < heap_size) {
            s = s + _p(_getd(h, i_1));
            if (i_1 < heap_size - 1) {
                s = s + ", ";
            }
            i_1 = i_1 + 1;
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            heap = ((double[])(new double[]{103.0, 9.0, 1.0, 7.0, 11.0, 15.0, 25.0, 201.0, 209.0, 107.0, 5.0}));
            size_1 = build_max_heap(((double[])(heap)));
            System.out.println(heap_to_string(((double[])(heap)), size_1));
            m = extract_max(((double[])(heap)), size_1);
            size_1 = size_1 - 1;
            System.out.println(_p(m));
            System.out.println(heap_to_string(((double[])(heap)), size_1));
            size_1 = insert(((double[])(heap)), size_1, 100.0);
            System.out.println(heap_to_string(((double[])(heap)), size_1));
            heap_sort(((double[])(heap)), size_1);
            System.out.println(heap_to_string(((double[])(heap)), size_1));
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
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
