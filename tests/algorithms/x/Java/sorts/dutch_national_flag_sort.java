public class Main {

    static int[] dutch_national_flag_sort(int[] seq) {
        int[] a = ((int[])(seq));
        int low = 0;
        int mid = 0;
        int high = a.length - 1;
        while (mid <= high) {
            int v = a[mid];
            if (v == 0) {
                int tmp = a[low];
a[low] = v;
a[mid] = tmp;
                low = low + 1;
                mid = mid + 1;
            } else             if (v == 1) {
                mid = mid + 1;
            } else             if (v == 2) {
                int tmp2 = a[high];
a[high] = v;
a[mid] = tmp2;
                high = high - 1;
            } else {
                throw new RuntimeException(String.valueOf("The elements inside the sequence must contains only (0, 1, 2) values"));
            }
        }
        return a;
    }
    public static void main(String[] args) {
        System.out.println(dutch_national_flag_sort(((int[])(new int[]{}))));
        System.out.println(dutch_national_flag_sort(((int[])(new int[]{0}))));
        System.out.println(dutch_national_flag_sort(((int[])(new int[]{2, 1, 0, 0, 1, 2}))));
        System.out.println(dutch_national_flag_sort(((int[])(new int[]{0, 1, 1, 0, 1, 2, 1, 2, 0, 0, 0, 1}))));
    }
}
