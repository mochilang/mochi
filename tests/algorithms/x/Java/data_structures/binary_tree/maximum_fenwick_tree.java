public class Main {
    static int[] arr = new int[0];

    static int[] zeros(int n) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        return res;
    }

    static void update(int[] arr, int idx, int value) {
arr[idx] = value;
    }

    static int query(int[] arr, int left, int right) {
        int result = 0;
        int i_1 = left;
        while (i_1 < right) {
            if (arr[i_1] > result) {
                result = arr[i_1];
            }
            i_1 = i_1 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        arr = ((int[])(new int[]{0, 0, 0, 0, 0}));
        System.out.println(query(((int[])(arr)), 0, 5));
        update(((int[])(arr)), 4, 100);
        System.out.println(query(((int[])(arr)), 0, 5));
        update(((int[])(arr)), 4, 0);
        update(((int[])(arr)), 2, 20);
        System.out.println(query(((int[])(arr)), 0, 5));
        update(((int[])(arr)), 4, 10);
        System.out.println(query(((int[])(arr)), 2, 5));
        System.out.println(query(((int[])(arr)), 1, 5));
        update(((int[])(arr)), 2, 0);
        System.out.println(query(((int[])(arr)), 0, 5));
        arr = ((int[])(zeros(10000)));
        update(((int[])(arr)), 255, 30);
        System.out.println(query(((int[])(arr)), 0, 10000));
        arr = ((int[])(zeros(6)));
        update(((int[])(arr)), 5, 1);
        System.out.println(query(((int[])(arr)), 5, 6));
        arr = ((int[])(zeros(6)));
        update(((int[])(arr)), 0, 1000);
        System.out.println(query(((int[])(arr)), 0, 1));
    }
}
