public class Main {

    static int search(int[] list_data, int key, int left, int right) {
        int r = right;
        if (r == 0) {
            r = list_data.length - 1;
        }
        if (left > r) {
            return -1;
        } else         if (list_data[left] == key) {
            return left;
        } else         if (list_data[r] == key) {
            return r;
        } else {
            return search(((int[])(list_data)), key, left + 1, r - 1);
        }
    }

    static void main() {
        System.out.println(search(((int[])(new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10})), 5, 0, 0));
        System.out.println(search(((int[])(new int[]{1, 2, 4, 5, 3})), 4, 0, 0));
        System.out.println(search(((int[])(new int[]{1, 2, 4, 5, 3})), 6, 0, 0));
        System.out.println(search(((int[])(new int[]{5})), 5, 0, 0));
        System.out.println(search(((int[])(new int[]{})), 1, 0, 0));
    }
    public static void main(String[] args) {
        main();
    }
}
