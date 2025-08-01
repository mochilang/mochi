public class Main {

    static java.util.function.Function<Double,Double> multiplier(double n1, double n2) {
        double n1n2 = n1 * n2;
        return (m) -> n1n2 * m;
    }

    static void main() {
        double x = 2.0;
        double xi = 0.5;
        double y = 4.0;
        double yi = 0.25;
        double z = x + y;
        double zi = 1.0 / (x + y);
        double[] numbers = new double[]{x, y, z};
        double[] inverses = new double[]{xi, yi, zi};
        java.util.function.Function<Double,Double>[] mfs = (java.util.function.Function<Double,Double>[])new java.util.function.Function[]{};
        int i = 0;
        while (i < numbers.length) {
            mfs = appendObj(mfs, multiplier(numbers[i], inverses[i]));
            i = i + 1;
        }
        for (java.util.function.Function<Double,Double> mf : mfs) {
            System.out.println(String.valueOf(mf.apply(1.0)));
        }
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
