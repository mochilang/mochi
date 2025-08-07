public class Main {
    static int[] data = new int[0];
    static int[] result = new int[0];

    static void heapify(int[] arr, int index, int heap_size) {
        int largest = index;
        int left_index = 2 * index + 1;
        int right_index = 2 * index + 2;
        if (left_index < heap_size && arr[left_index] > arr[largest]) {
            largest = left_index;
        }
        if (right_index < heap_size && arr[right_index] > arr[largest]) {
            largest = right_index;
        }
        if (largest != index) {
            int temp = arr[largest];
arr[largest] = arr[index];
arr[index] = temp;
            heapify(((int[])(arr)), largest, heap_size);
        }
    }

    static int[] heap_sort(int[] arr) {
        int n = arr.length;
        int i = Math.floorDiv(n, 2) - 1;
        while (i >= 0) {
            heapify(((int[])(arr)), i, n);
            i = i - 1;
        }
        i = n - 1;
        while (i > 0) {
            int temp_1 = arr[0];
arr[0] = arr[i];
arr[i] = temp_1;
            heapify(((int[])(arr)), 0, i);
            i = i - 1;
        }
        return arr;
    }
    public static void main(String[] args) {
        data = ((int[])(new int[]{3, 7, 9, 28, 123, -5, 8, -30, -200, 0, 4}));
        result = ((int[])(heap_sort(((int[])(data)))));
        System.out.println(java.util.Arrays.toString(result));
        if (!(_p(result).equals(_p(new int[]{-200, -30, -5, 0, 3, 4, 7, 8, 9, 28, 123})))) {
            throw new RuntimeException(String.valueOf("Assertion error"));
        }
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
