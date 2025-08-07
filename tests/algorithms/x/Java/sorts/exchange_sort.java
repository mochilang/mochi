public class Main {

    static int[] exchange_sort(int[] numbers) {
        int n = numbers.length;
        int i = 0;
        while (i < n) {
            int j = i + 1;
            while (j < n) {
                if (numbers[j] < numbers[i]) {
                    int temp = numbers[i];
numbers[i] = numbers[j];
numbers[j] = temp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return numbers;
    }
    public static void main(String[] args) {
        System.out.println(_p(exchange_sort(((int[])(new int[]{5, 4, 3, 2, 1})))));
        System.out.println(_p(exchange_sort(((int[])(new int[]{-1, -2, -3})))));
        System.out.println(_p(exchange_sort(((int[])(new int[]{1, 2, 3, 4, 5})))));
        System.out.println(_p(exchange_sort(((int[])(new int[]{0, 10, -2, 5, 3})))));
        System.out.println(_p(exchange_sort(((int[])(new int[]{})))));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
