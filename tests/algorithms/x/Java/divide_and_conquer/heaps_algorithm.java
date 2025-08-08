public class Main {

    static int[][] permute(int k, int[] arr, int[][] res) {
        if (k == 1) {
            int[] copy = ((int[])(java.util.Arrays.copyOfRange(arr, 0, arr.length)));
            return appendObj((int[][])res, copy);
        }
        res = ((int[][])(permute(k - 1, ((int[])(arr)), ((int[][])(res)))));
        int i = 0;
        while (i < k - 1) {
            if (Math.floorMod(k, 2) == 0) {
                int temp = arr[i];
arr[i] = arr[k - 1];
arr[k - 1] = temp;
            } else {
                int temp_1 = arr[0];
arr[0] = arr[k - 1];
arr[k - 1] = temp_1;
            }
            res = ((int[][])(permute(k - 1, ((int[])(arr)), ((int[][])(res)))));
            i = i + 1;
        }
        return res;
    }

    static int[][] heaps(int[] arr) {
        if (arr.length <= 1) {
            return new int[][]{java.util.Arrays.copyOfRange(arr, 0, arr.length)};
        }
        int[][] res = ((int[][])(new int[][]{}));
        res = ((int[][])(permute(arr.length, ((int[])(arr)), ((int[][])(res)))));
        return res;
    }

    static void main() {
        int[][] perms = ((int[][])(heaps(((int[])(new int[]{1, 2, 3})))));
        System.out.println(java.util.Arrays.deepToString(perms));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
