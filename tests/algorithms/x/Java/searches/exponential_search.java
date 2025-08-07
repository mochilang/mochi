public class Main {
    static boolean is_sorted(int[] xs) {
        int i = 1;
        while (i < xs.length) {
            if (xs[i - 1] > xs[i]) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static int exponential_search(int[] arr, int item) {
        if (!(Boolean)is_sorted(((int[])(arr)))) {
            throw new RuntimeException(String.valueOf("sorted_collection must be sorted in ascending order"));
        }
        if (arr.length == 0) {
            return -1;
        }
        if (arr[0] == item) {
            return 0;
        }
        int bound = 1;
        while (bound < arr.length && arr[bound] < item) {
            bound = bound * 2;
        }
        int left = Math.floorDiv(bound, 2);
        int right = bound;
        if (right >= arr.length) {
            right = arr.length - 1;
        }
        while (left <= right) {
            int mid = left + Math.floorDiv((right - left), 2);
            if (arr[mid] == item) {
                return mid;
            }
            if (arr[mid] > item) {
                right = mid - 1;
            } else {
                left = mid + 1;
            }
        }
        return -1;
    }
    public static void main(String[] args) {
    }
}
