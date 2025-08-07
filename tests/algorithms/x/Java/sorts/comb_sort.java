public class Main {

    static int[] comb_sort(int[] data) {
        double shrink_factor = 1.3;
        int gap = data.length;
        boolean completed = false;
        while (!completed) {
            gap = ((Number)(gap / shrink_factor)).intValue();
            if (gap <= 1) {
                gap = 1;
                completed = true;
            }
            int index = 0;
            while (index + gap < data.length) {
                if (data[index] > data[index + gap]) {
                    int tmp = data[index];
data[index] = data[index + gap];
data[index + gap] = tmp;
                    completed = false;
                }
                index = index + 1;
            }
        }
        return data;
    }

    static void main() {
        System.out.println(comb_sort(((int[])(new int[]{0, 5, 3, 2, 2}))));
        System.out.println(comb_sort(((int[])(new int[]{}))));
        System.out.println(comb_sort(((int[])(new int[]{99, 45, -7, 8, 2, 0, -15, 3}))));
    }
    public static void main(String[] args) {
        main();
    }
}
