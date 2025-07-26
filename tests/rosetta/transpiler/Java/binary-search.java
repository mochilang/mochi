public class Main {

    static int bsearch(int[] arr, int x) {
        int low = 0;
        int high = arr.length - 1;
        while (low <= high) {
            int mid = (low + high) / 2;
            if (arr[mid] > x) {
                high = mid - 1;
            } else             if (arr[mid] < x) {
                low = mid + 1;
            } else {
                return mid;
            }
        }
        return -1;
    }

    static int bsearchRec(int[] arr, int x, int low, int high) {
        if (high < low) {
            return -1;
        }
        int mid = (low + high) / 2;
        if (arr[mid] > x) {
            return bsearchRec(arr, x, low, mid - 1);
        } else         if (arr[mid] < x) {
            return bsearchRec(arr, x, mid + 1, high);
        }
        return mid;
    }

    static void main() {
        int[] nums = new int[]{-31, 0, 1, 2, 2, 4, 65, 83, 99, 782};
        int x = 2;
        int idx = bsearch(nums, x);
        if (idx >= 0) {
            System.out.println(String.valueOf(x) + " is at index " + String.valueOf(idx) + ".");
        } else {
            System.out.println(String.valueOf(x) + " is not found.");
        }
        x = 5;
        idx = bsearchRec(nums, x, 0, nums.length - 1);
        if (idx >= 0) {
            System.out.println(String.valueOf(x) + " is at index " + String.valueOf(idx) + ".");
        } else {
            System.out.println(String.valueOf(x) + " is not found.");
        }
    }
    public static void main(String[] args) {
        main();
    }
}
