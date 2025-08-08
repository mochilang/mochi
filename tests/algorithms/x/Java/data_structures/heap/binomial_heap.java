public class Main {
    static class BinomialHeap {
        int[] data;
        BinomialHeap(int[] data) {
            this.data = data;
        }
        BinomialHeap() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }

    static class DeleteResult {
        BinomialHeap heap;
        int value;
        DeleteResult(BinomialHeap heap, int value) {
            this.heap = heap;
            this.value = value;
        }
        DeleteResult() {}
        @Override public String toString() {
            return String.format("{'heap': %s, 'value': %s}", String.valueOf(heap), String.valueOf(value));
        }
    }


    static BinomialHeap new_heap() {
        return new BinomialHeap(new int[]{});
    }

    static void swap(int[] data, int i, int j) {
        int tmp = data[i];
data[i] = data[j];
data[j] = tmp;
    }

    static void sift_up(int[] data, int idx) {
        int i = idx;
        while (i > 0) {
            int parent = Math.floorDiv((i - 1), 2);
            if (data[parent] <= data[i]) {
                break;
            }
            swap(((int[])(data)), parent, i);
            i = parent;
        }
    }

    static void sift_down(int[] data, int idx) {
        int i_1 = idx;
        int n = data.length;
        while (true) {
            int left = 2 * i_1 + 1;
            int right = left + 1;
            int smallest = i_1;
            if (left < n && data[left] < data[smallest]) {
                smallest = left;
            }
            if (right < n && data[right] < data[smallest]) {
                smallest = right;
            }
            if (smallest == i_1) {
                break;
            }
            swap(((int[])(data)), i_1, smallest);
            i_1 = smallest;
        }
    }

    static BinomialHeap insert(BinomialHeap heap, int v) {
        int[] d = ((int[])(heap.data));
        d = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(d), java.util.stream.IntStream.of(v)).toArray()));
        sift_up(((int[])(d)), d.length - 1);
        return new BinomialHeap(d);
    }

    static int peek(BinomialHeap heap) {
        return heap.data[0];
    }

    static boolean is_empty(BinomialHeap heap) {
        return heap.data.length == 0;
    }

    static DeleteResult delete_min(BinomialHeap heap) {
        int[] d_1 = ((int[])(heap.data));
        int min = d_1[0];
d_1[0] = d_1[d_1.length - 1];
        d_1 = ((int[])(java.util.Arrays.copyOfRange(d_1, 0, d_1.length - 1)));
        if (d_1.length > 0) {
            sift_down(((int[])(d_1)), 0);
        }
        return new DeleteResult(new BinomialHeap(d_1), min);
    }

    static void main() {
        BinomialHeap h = new_heap();
        h = insert(h, 10);
        h = insert(h, 3);
        h = insert(h, 7);
        System.out.println(_p(peek(h)));
        DeleteResult d1 = delete_min(h);
        h = d1.heap;
        System.out.println(_p(d1.value));
        DeleteResult d2 = delete_min(h);
        h = d2.heap;
        System.out.println(_p(d2.value));
        DeleteResult d3 = delete_min(h);
        h = d3.heap;
        System.out.println(_p(d3.value));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
}
