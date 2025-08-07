public class Main {
    static int[] arr1 = new int[0];
    static int[] arr2 = new int[0];
    static int[] arr3 = new int[0];
    static int[] arr4 = new int[0];
    static int[] arr5 = new int[0];

    static boolean binary_search(int[] arr, int item) {
        int low = 0;
        int high = arr.length - 1;
        while (low <= high) {
            int mid = Math.floorDiv((low + high), 2);
            int val = arr[mid];
            if (val == item) {
                return true;
            }
            if (item < val) {
                high = mid - 1;
            } else {
                low = mid + 1;
            }
        }
        return false;
    }
    public static void main(String[] args) {
        arr1 = ((int[])(new int[]{0, 1, 2, 8, 13, 17, 19, 32, 42}));
        System.out.println(binary_search(((int[])(arr1)), 3));
        System.out.println(binary_search(((int[])(arr1)), 13));
        arr2 = ((int[])(new int[]{4, 4, 5, 6, 7}));
        System.out.println(binary_search(((int[])(arr2)), 4));
        System.out.println(binary_search(((int[])(arr2)), -10));
        arr3 = ((int[])(new int[]{-18, 2}));
        System.out.println(binary_search(((int[])(arr3)), -18));
        arr4 = ((int[])(new int[]{5}));
        System.out.println(binary_search(((int[])(arr4)), 5));
        arr5 = ((int[])(new int[]{}));
        System.out.println(binary_search(((int[])(arr5)), 1));
    }
}
