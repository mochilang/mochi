public class Main {
    static int[] heap = new int[0];
    static int size = 0;

    static void swap_up(int i) {
        int temp = heap[i];
        int idx = i;
        while (Math.floorDiv(idx, 2) > 0) {
            if (heap[idx] > heap[Math.floorDiv(idx, 2)]) {
heap[idx] = heap[Math.floorDiv(idx, 2)];
heap[Math.floorDiv(idx, 2)] = temp;
            }
            idx = Math.floorDiv(idx, 2);
        }
    }

    static void insert(int value) {
        heap = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(heap), java.util.stream.IntStream.of(value)).toArray()));
        size = size + 1;
        swap_up(size);
    }

    static void swap_down(int i) {
        int idx_1 = i;
        while (size >= 2 * idx_1) {
            int bigger_child = 2 * idx_1 + 1 > size ? 2 * idx_1 : heap[2 * idx_1] > heap[2 * idx_1 + 1] ? 2 * idx_1 : 2 * idx_1 + 1;
            int temp_1 = heap[idx_1];
            if (heap[idx_1] < heap[bigger_child]) {
heap[idx_1] = heap[bigger_child];
heap[bigger_child] = temp_1;
            }
            idx_1 = bigger_child;
        }
    }

    static void shrink() {
        int[] new_heap = ((int[])(new int[]{}));
        int i = 0;
        while (i <= size) {
            new_heap = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_heap), java.util.stream.IntStream.of(heap[i])).toArray()));
            i = i + 1;
        }
        heap = ((int[])(new_heap));
    }

    static int pop() {
        int max_value = heap[1];
heap[1] = heap[size];
        size = size - 1;
        shrink();
        swap_down(1);
        return max_value;
    }

    static int[] get_list() {
        int[] out = ((int[])(new int[]{}));
        int i_1 = 1;
        while (i_1 <= size) {
            out = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(heap[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return out;
    }

    static int len() {
        return size;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            heap = ((int[])(new int[]{0}));
            size = 0;
            insert(6);
            insert(10);
            insert(15);
            insert(12);
            System.out.println(pop());
            System.out.println(pop());
            System.out.println(get_list());
            System.out.println(len());
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
