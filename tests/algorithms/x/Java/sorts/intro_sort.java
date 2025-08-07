public class Main {
    static int[] example1;
    static int[] example2;

    static int[] insertion_sort(int[] a, int start, int end_) {
        int[] arr = ((int[])(a));
        int i = start;
        while (i < end_) {
            int key = arr[i];
            int j = i;
            while (j > start && arr[j - 1] > key) {
arr[j] = arr[j - 1];
                j = j - 1;
            }
arr[j] = key;
            i = i + 1;
        }
        return arr;
    }

    static int[] heapify(int[] a, int index, int heap_size) {
        int[] arr_1 = ((int[])(a));
        int largest = index;
        int left = 2 * index + 1;
        int right = 2 * index + 2;
        if (left < heap_size && arr_1[left] > arr_1[largest]) {
            largest = left;
        }
        if (right < heap_size && arr_1[right] > arr_1[largest]) {
            largest = right;
        }
        if (largest != index) {
            int temp = arr_1[index];
arr_1[index] = arr_1[largest];
arr_1[largest] = temp;
            arr_1 = ((int[])(heapify(((int[])(arr_1)), largest, heap_size)));
        }
        return arr_1;
    }

    static int[] heap_sort(int[] a) {
        int[] arr_2 = ((int[])(a));
        int n = arr_2.length;
        if (n <= 1) {
            return arr_2;
        }
        int i_1 = Math.floorDiv(n, 2);
        while (true) {
            arr_2 = ((int[])(heapify(((int[])(arr_2)), i_1, n)));
            if (i_1 == 0) {
                break;
            }
            i_1 = i_1 - 1;
        }
        i_1 = n - 1;
        while (i_1 > 0) {
            int temp_1 = arr_2[0];
arr_2[0] = arr_2[i_1];
arr_2[i_1] = temp_1;
            arr_2 = ((int[])(heapify(((int[])(arr_2)), 0, i_1)));
            i_1 = i_1 - 1;
        }
        return arr_2;
    }

    static int median_of_3(int[] arr, int first, int middle, int last) {
        int a = arr[first];
        int b = arr[middle];
        int c = arr[last];
        if ((a > b && a < c) || (a < b && a > c)) {
            return a;
        } else         if ((b > a && b < c) || (b < a && b > c)) {
            return b;
        } else {
            return c;
        }
    }

    static int partition(int[] arr, int low, int high, int pivot) {
        int i_2 = low;
        int j_1 = high;
        while (true) {
            while (arr[i_2] < pivot) {
                i_2 = i_2 + 1;
            }
            j_1 = j_1 - 1;
            while (pivot < arr[j_1]) {
                j_1 = j_1 - 1;
            }
            if (i_2 >= j_1) {
                return i_2;
            }
            int temp_2 = arr[i_2];
arr[i_2] = arr[j_1];
arr[j_1] = temp_2;
            i_2 = i_2 + 1;
        }
    }

    static int int_log2(int n) {
        int v = n;
        int r = 0;
        while (v > 1) {
            v = Math.floorDiv(v, 2);
            r = r + 1;
        }
        return r;
    }

    static int[] intro_sort(int[] arr, int start, int end_, int size_threshold, int max_depth) {
        int[] array = ((int[])(arr));
        int s = start;
        int e = end_;
        int depth = max_depth;
        while (e - s > size_threshold) {
            if (depth == 0) {
                return heap_sort(((int[])(array)));
            }
            depth = depth - 1;
            int pivot = median_of_3(((int[])(array)), s, s + ((Number)((Math.floorDiv((e - s), 2)))).intValue() + 1, e - 1);
            int p = partition(((int[])(array)), s, e, pivot);
            array = ((int[])(intro_sort(((int[])(array)), p, e, size_threshold, depth)));
            e = p;
        }
        int[] res = ((int[])(insertion_sort(((int[])(array)), s, e)));
        int _v = res.length;
        return res;
    }

    static void intro_sort_main(int[] arr) {
        if (arr.length == 0) {
            System.out.println(java.util.Arrays.toString(arr));
            return;
        }
        int max_depth = 2 * int_log2(arr.length);
        int[] sorted = ((int[])(intro_sort(((int[])(arr)), 0, arr.length, 16, max_depth)));
        System.out.println(java.util.Arrays.toString(sorted));
    }
    public static void main(String[] args) {
        example1 = ((int[])(new int[]{4, 2, 6, 8, 1, 7, 8, 22, 14, 56, 27, 79, 23, 45, 14, 12}));
        intro_sort_main(((int[])(example1)));
        example2 = ((int[])(new int[]{21, 15, 11, 45, -2, -11, 46}));
        intro_sort_main(((int[])(example2)));
    }
}
