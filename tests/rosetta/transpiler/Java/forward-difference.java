public class Main {
    static int[] a;

    static int[] fd(int[] a, int ord) {
        int i = 0;
        while (i < ord) {
            int j = 0;
            while (j < a.length - i - 1) {
a[j] = a[j + 1] - a[j];
                j = j + 1;
            }
            i = i + 1;
        }
        return java.util.Arrays.copyOfRange(a, 0, a.length - ord);
    }
    public static void main(String[] args) {
        a = ((int[])(new int[]{90, 47, 58, 29, 22, 32, 55, 5, 55, 73}));
        System.out.println(_p(a));
        System.out.println(_p(fd(((int[])(a)), 9)));
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
