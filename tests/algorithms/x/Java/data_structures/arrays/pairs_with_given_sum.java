public class Main {

    static int pairs_with_sum(int[] arr, int req_sum) {
        int n = arr.length;
        int count = 0;
        int i = 0;
        while (i < n) {
            int j = i + 1;
            while (j < n) {
                if (arr[i] + arr[j] == req_sum) {
                    count = count + 1;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return count;
    }
    public static void main(String[] args) {
        System.out.println(pairs_with_sum(((int[])(new int[]{1, 5, 7, 1})), 6));
        System.out.println(pairs_with_sum(((int[])(new int[]{1, 1, 1, 1, 1, 1, 1, 1})), 2));
        System.out.println(pairs_with_sum(((int[])(new int[]{1, 7, 6, 2, 5, 4, 3, 1, 9, 8})), 7));
    }
}
