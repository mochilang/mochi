public class Main {
    static java.util.function.Supplier<Integer> fibNumber() {
        int[] a = new int[1];
        a[0] = 0;
        int[] b = new int[1];
        b[0] = 1;
        return () -> {
        int tmp = a[0] + b[0];
        a[0] = b[0];
        b[0] = tmp;
        return a[0];
};
    }

    static int fibSequence(int n) {
        java.util.function.Supplier<Integer> f = fibNumber();
        int r = 0;
        int i = 0;
        while (i < n) {
            r = ((Number)(f.get())).intValue();
            i = i + 1;
        }
        return r;
    }
    public static void main(String[] args) {
    }
}
