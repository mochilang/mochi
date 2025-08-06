public class Main {
    static int[] arr1;
    static int[] arr2;

    static int partition(int[] arr, int low, int high) {
        int pivot = arr[high];
        int i = low - 1;
        int j = low;
        while (j < high) {
            if (arr[j] >= pivot) {
                i = i + 1;
                int tmp = arr[i];
arr[i] = arr[j];
arr[j] = tmp;
            }
            j = j + 1;
        }
        int k = i + 1;
        int tmp_1 = arr[k];
arr[k] = arr[high];
arr[high] = tmp_1;
        return k;
    }

    static int kth_largest_element(int[] arr, int position) {
        if (arr.length == 0) {
            return -1;
        }
        if (position < 1 || position > arr.length) {
            return -1;
        }
        int low = 0;
        int high = arr.length - 1;
        while (low <= high) {
            if (low > arr.length - 1 || high < 0) {
                return -1;
            }
            int pivot_index = partition(((int[])(arr)), low, high);
            if (pivot_index == position - 1) {
                return arr[pivot_index];
            } else             if (pivot_index > position - 1) {
                high = pivot_index - 1;
            } else {
                low = pivot_index + 1;
            }
        }
        return -1;
    }
    public static void main(String[] args) {
        arr1 = ((int[])(new int[]{3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5}));
        System.out.println(kth_largest_element(((int[])(arr1)), 3));
        System.out.println("\n");
        arr2 = ((int[])(new int[]{2, 5, 6, 1, 9, 3, 8, 4, 7, 3, 5}));
        System.out.println(kth_largest_element(((int[])(arr2)), 1));
    }
}
