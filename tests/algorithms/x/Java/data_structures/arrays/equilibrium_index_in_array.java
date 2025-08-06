public class Main {
    static int[] arr1 = new int[0];
    static int[] arr2 = new int[0];
    static int[] arr3 = new int[0];
    static int[] arr4 = new int[0];

    static int equilibrium_index(int[] arr) {
        int total = 0;
        int i = 0;
        while (i < arr.length) {
            total = total + arr[i];
            i = i + 1;
        }
        int left = 0;
        i = 0;
        while (i < arr.length) {
            total = total - arr[i];
            if (left == total) {
                return i;
            }
            left = left + arr[i];
            i = i + 1;
        }
        return -1;
    }
    public static void main(String[] args) {
        arr1 = ((int[])(new int[]{-7, 1, 5, 2, -4, 3, 0}));
        System.out.println(equilibrium_index(((int[])(arr1))));
        arr2 = ((int[])(new int[]{1, 2, 3, 4, 5}));
        System.out.println(equilibrium_index(((int[])(arr2))));
        arr3 = ((int[])(new int[]{1, 1, 1, 1, 1}));
        System.out.println(equilibrium_index(((int[])(arr3))));
        arr4 = ((int[])(new int[]{2, 4, 6, 8, 10, 3}));
        System.out.println(equilibrium_index(((int[])(arr4))));
    }
}
