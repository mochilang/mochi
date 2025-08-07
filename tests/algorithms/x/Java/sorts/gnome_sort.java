public class Main {

    static int[] gnome_sort(int[] lst) {
        if (lst.length <= 1) {
            return lst;
        }
        int i = 1;
        while (i < lst.length) {
            if (lst[i - 1] <= lst[i]) {
                i = i + 1;
            } else {
                int tmp = lst[i - 1];
lst[i - 1] = lst[i];
lst[i] = tmp;
                i = i - 1;
                if (i == 0) {
                    i = 1;
                }
            }
        }
        return lst;
    }
    public static void main(String[] args) {
        System.out.println(gnome_sort(((int[])(new int[]{0, 5, 3, 2, 2}))));
        System.out.println(gnome_sort(((int[])(new int[]{}))));
        System.out.println(gnome_sort(((int[])(new int[]{-2, -5, -45}))));
    }
}
