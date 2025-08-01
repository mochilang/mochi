public class Main {
    static double[] x1;
    static double[] x2;
    static double[] x3;

    static double[] sortFloat(double[] xs) {
        double[] arr = xs;
        int n = arr.length;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < n - 1) {
                if (arr[j] > arr[j + 1]) {
                    double t = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = t;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static double median(double[] s) {
        int n_1 = s.length;
        if (Math.floorMod(n_1, 2) == 1) {
            return s[n_1 / 2];
        }
        return (s[n_1 / 2 - 1] + s[n_1 / 2]) / 2.0;
    }

    static double[] fivenum(double[] xs) {
        double[] s = sortFloat(xs);
        int n_2 = s.length;
        double q1 = s[(n_2 - 1) / 4];
        double med = median(s);
        double q3 = s[(3 * (n_2 - 1)) / 4];
        return new double[]{s[0], q1, med, q3, s[n_2 - 1]};
    }
    public static void main(String[] args) {
        x1 = new double[]{36.0, 40.0, 7.0, 39.0, 41.0, 15.0};
        x2 = new double[]{15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0};
        x3 = new double[]{0.14082834, 0.0974879, 1.73131507, 0.87636009, -1.95059594, 0.73438555, -0.03035726, 1.4667597, -0.74621349, -0.72588772, 0.6390516, 0.61501527, -0.9898378, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578};
        System.out.println(String.valueOf(fivenum(x1)));
        System.out.println(String.valueOf(fivenum(x2)));
        System.out.println(String.valueOf(fivenum(x3)));
    }
}
