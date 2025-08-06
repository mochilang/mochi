public class Main {
    static int[] heap = new int[0];
    static int size = 0;

    static void swap_up(int i) {
        int temp = heap[i];
        int idx = i;
        while (idx / 2 > 0) {
            if (heap[idx] > heap[idx / 2]) {
heap[idx] = heap[idx / 2];
heap[idx / 2] = temp;
            }
            idx = idx / 2;
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
    }
}
