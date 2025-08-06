public class Main {
    static double PI;
    static double TWO_PI;
    static int seed = 0;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (((Number)(rand())).doubleValue()) / 2147483648.0;
    }

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double cos(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2 = y * y;
        double y4 = y2 * y2;
        double y6 = y4 * y2;
        return 1.0 - y2 / 2.0 + y4 / 24.0 - y6 / 720.0;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double ln(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term = t;
        double sum = 0.0;
        int n = 1;
        while (n <= 19) {
            sum = sum + term / (((Number)(n)).doubleValue());
            term = term * t * t;
            n = n + 2;
        }
        return 2.0 * sum;
    }

    static double[] gaussian_distribution(double mean, double std_dev, int instance_count) {
        double[] res = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < instance_count) {
            double u1 = random();
            double u2 = random();
            double r = sqrtApprox(-2.0 * ln(u1));
            double theta = TWO_PI * u2;
            double z = r * cos(theta);
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(mean + z * std_dev)).toArray()));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static int[] y_generator(int class_count, int[] instance_count) {
        int[] res_1 = ((int[])(new int[]{}));
        int k = 0;
        while (k < class_count) {
            int i_2 = 0;
            while (i_2 < instance_count[k]) {
                res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(k)).toArray()));
                i_2 = i_2 + 1;
            }
            k = k + 1;
        }
        return res_1;
    }

    static double calculate_mean(int instance_count, double[] items) {
        double total = 0.0;
        int i_3 = 0;
        while (i_3 < instance_count) {
            total = total + items[i_3];
            i_3 = i_3 + 1;
        }
        return total / (((Number)(instance_count)).doubleValue());
    }

    static double calculate_probabilities(int instance_count, int total_count) {
        return (((Number)(instance_count)).doubleValue()) / (((Number)(total_count)).doubleValue());
    }

    static double calculate_variance(double[][] items, double[] means, int total_count) {
        double[] squared_diff = ((double[])(new double[]{}));
        int i_4 = 0;
        while (i_4 < items.length) {
            int j = 0;
            while (j < items[i_4].length) {
                double diff = items[i_4][j] - means[i_4];
                squared_diff = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(squared_diff), java.util.stream.DoubleStream.of(diff * diff)).toArray()));
                j = j + 1;
            }
            i_4 = i_4 + 1;
        }
        double sum_sq = 0.0;
        int k_1 = 0;
        while (k_1 < squared_diff.length) {
            sum_sq = sum_sq + squared_diff[k_1];
            k_1 = k_1 + 1;
        }
        int n_classes = means.length;
        return (1.0 / (((Number)((total_count - n_classes))).doubleValue())) * sum_sq;
    }

    static int[] predict_y_values(double[][] x_items, double[] means, double variance, double[] probabilities) {
        int[] results = ((int[])(new int[]{}));
        int i_5 = 0;
        while (i_5 < x_items.length) {
            int j_1 = 0;
            while (j_1 < x_items[i_5].length) {
                double[] temp = ((double[])(new double[]{}));
                int k_2 = 0;
                while (k_2 < x_items.length) {
                    double discr = x_items[i_5][j_1] * (means[k_2] / variance) - (means[k_2] * means[k_2]) / (2.0 * variance) + ln(probabilities[k_2]);
                    temp = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(temp), java.util.stream.DoubleStream.of(discr)).toArray()));
                    k_2 = k_2 + 1;
                }
                int max_idx = 0;
                double max_val = temp[0];
                int t_1 = 1;
                while (t_1 < temp.length) {
                    if (temp[t_1] > max_val) {
                        max_val = temp[t_1];
                        max_idx = t_1;
                    }
                    t_1 = t_1 + 1;
                }
                results = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(results), java.util.stream.IntStream.of(max_idx)).toArray()));
                j_1 = j_1 + 1;
            }
            i_5 = i_5 + 1;
        }
        return results;
    }

    static double accuracy(int[] actual_y, int[] predicted_y) {
        int correct = 0;
        int i_6 = 0;
        while (i_6 < actual_y.length) {
            if (actual_y[i_6] == predicted_y[i_6]) {
                correct = correct + 1;
            }
            i_6 = i_6 + 1;
        }
        return (((Number)(correct)).doubleValue()) / (((Number)(actual_y.length)).doubleValue()) * 100.0;
    }

    static void main() {
        seed = 1;
        int[] counts = ((int[])(new int[]{20, 20, 20}));
        double[] means = ((double[])(new double[]{5.0, 10.0, 15.0}));
        double std_dev = 1.0;
        double[][] x = ((double[][])(new double[][]{}));
        int i_7 = 0;
        while (i_7 < counts.length) {
            x = ((double[][])(appendObj(x, gaussian_distribution(means[i_7], std_dev, counts[i_7]))));
            i_7 = i_7 + 1;
        }
        int[] y_1 = ((int[])(y_generator(counts.length, ((int[])(counts)))));
        double[] actual_means = ((double[])(new double[]{}));
        i_7 = 0;
        while (i_7 < counts.length) {
            actual_means = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(actual_means), java.util.stream.DoubleStream.of(calculate_mean(counts[i_7], ((double[])(x[i_7]))))).toArray()));
            i_7 = i_7 + 1;
        }
        int total_count = 0;
        i_7 = 0;
        while (i_7 < counts.length) {
            total_count = total_count + counts[i_7];
            i_7 = i_7 + 1;
        }
        double[] probabilities = ((double[])(new double[]{}));
        i_7 = 0;
        while (i_7 < counts.length) {
            probabilities = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(probabilities), java.util.stream.DoubleStream.of(calculate_probabilities(counts[i_7], total_count))).toArray()));
            i_7 = i_7 + 1;
        }
        double variance = calculate_variance(((double[][])(x)), ((double[])(actual_means)), total_count);
        int[] predicted = ((int[])(predict_y_values(((double[][])(x)), ((double[])(actual_means)), variance, ((double[])(probabilities)))));
        System.out.println(java.util.Arrays.toString(predicted));
        System.out.println(accuracy(((int[])(y_1)), ((int[])(predicted))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            TWO_PI = 6.283185307179586;
            seed = 1;
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
