public class Main {
    static double PI;
    static int sample_rate;
    static int size;
    static double[] audio = new double[0];
    static int n_5 = 0;
    static double[] coeffs;

    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 10) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double cosApprox(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int n_1 = 1;
        while (n_1 <= 10) {
            double denom_1 = ((Number)(((2 * n_1 - 1) * (2 * n_1)))).doubleValue();
            term_1 = -term_1 * x * x / denom_1;
            sum_1 = sum_1 + term_1;
            n_1 = n_1 + 1;
        }
        return sum_1;
    }

    static double expApprox(double x) {
        double sum_2 = 1.0;
        double term_2 = 1.0;
        int n_2 = 1;
        while (n_2 < 10) {
            term_2 = term_2 * x / (((Number)(n_2)).doubleValue());
            sum_2 = sum_2 + term_2;
            n_2 = n_2 + 1;
        }
        return sum_2;
    }

    static double ln(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term_3 = t;
        double sum_3 = 0.0;
        int n_3 = 1;
        while (n_3 <= 19) {
            sum_3 = sum_3 + term_3 / (((Number)(n_3)).doubleValue());
            term_3 = term_3 * t * t;
            n_3 = n_3 + 2;
        }
        return 2.0 * sum_3;
    }

    static double log10(double x) {
        return ln(x) / ln(10.0);
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

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double[] normalize(double[] audio) {
        double max_val = 0.0;
        int i_1 = 0;
        while (i_1 < audio.length) {
            double v = absf(audio[i_1]);
            if (v > max_val) {
                max_val = v;
            }
            i_1 = i_1 + 1;
        }
        double[] res = ((double[])(new double[]{}));
        i_1 = 0;
        while (i_1 < audio.length) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(audio[i_1] / max_val)).toArray()));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static double[] dft(double[] frame, int bins) {
        int N = frame.length;
        double[] spec = ((double[])(new double[]{}));
        int k = 0;
        while (k < bins) {
            double real = 0.0;
            double imag = 0.0;
            int n_4 = 0;
            while (n_4 < N) {
                double angle = -2.0 * PI * (((Number)(k)).doubleValue()) * (((Number)(n_4)).doubleValue()) / (((Number)(N)).doubleValue());
                real = real + frame[n_4] * cosApprox(angle);
                imag = imag + frame[n_4] * sinApprox(angle);
                n_4 = n_4 + 1;
            }
            spec = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(spec), java.util.stream.DoubleStream.of(real * real + imag * imag)).toArray()));
            k = k + 1;
        }
        return spec;
    }

    static double[][] triangular_filters(int bins, int spectrum_size) {
        double[][] filters = ((double[][])(new double[][]{}));
        int b = 0;
        while (b < bins) {
            int center = ((b + 1) * spectrum_size) / (bins + 1);
            double[] filt = ((double[])(new double[]{}));
            int i_2 = 0;
            while (i_2 < spectrum_size) {
                double v_1 = 0.0;
                if (i_2 <= center) {
                    v_1 = (((Number)(i_2)).doubleValue()) / (((Number)(center)).doubleValue());
                } else {
                    v_1 = (((Number)((spectrum_size - i_2))).doubleValue()) / (((Number)((spectrum_size - center))).doubleValue());
                }
                filt = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(filt), java.util.stream.DoubleStream.of(v_1)).toArray()));
                i_2 = i_2 + 1;
            }
            filters = ((double[][])(appendObj(filters, filt)));
            b = b + 1;
        }
        return filters;
    }

    static double[] dot(double[][] mat, double[] vec) {
        double[] res_1 = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < mat.length) {
            double sum_4 = 0.0;
            int j = 0;
            while (j < vec.length) {
                sum_4 = sum_4 + mat[i_3][j] * vec[j];
                j = j + 1;
            }
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(sum_4)).toArray()));
            i_3 = i_3 + 1;
        }
        return res_1;
    }

    static double[][] discrete_cosine_transform(int dct_filter_num, int filter_num) {
        double[][] basis = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < dct_filter_num) {
            double[] row = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < filter_num) {
                if (i_4 == 0) {
                    row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(1.0 / sqrtApprox(((Number)(filter_num)).doubleValue()))).toArray()));
                } else {
                    double angle_1 = (((Number)((2 * j_1 + 1))).doubleValue()) * (((Number)(i_4)).doubleValue()) * PI / (2.0 * (((Number)(filter_num)).doubleValue()));
                    row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(cosApprox(angle_1) * sqrtApprox(2.0 / (((Number)(filter_num)).doubleValue())))).toArray()));
                }
                j_1 = j_1 + 1;
            }
            basis = ((double[][])(appendObj(basis, row)));
            i_4 = i_4 + 1;
        }
        return basis;
    }

    static double[] mfcc(double[] audio, int bins, int dct_num) {
        double[] norm = ((double[])(normalize(((double[])(audio)))));
        double[] spec_1 = ((double[])(dft(((double[])(norm)), bins + 2)));
        double[][] filters_1 = ((double[][])(triangular_filters(bins, spec_1.length)));
        double[] energies = ((double[])(dot(((double[][])(filters_1)), ((double[])(spec_1)))));
        double[] logfb = ((double[])(new double[]{}));
        int i_5 = 0;
        while (i_5 < energies.length) {
            logfb = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(logfb), java.util.stream.DoubleStream.of(10.0 * log10(energies[i_5] + 1e-10))).toArray()));
            i_5 = i_5 + 1;
        }
        double[][] dct_basis = ((double[][])(discrete_cosine_transform(dct_num, bins)));
        double[] res_2 = ((double[])(dot(((double[][])(dct_basis)), ((double[])(logfb)))));
        if (res_2.length == 0) {
            res_2 = ((double[])(new double[]{0.0, 0.0, 0.0}));
        }
        return res_2;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            sample_rate = 8000;
            size = 16;
            audio = ((double[])(new double[]{}));
            n_5 = 0;
            while (n_5 < size) {
                double t_1 = (((Number)(n_5)).doubleValue()) / (((Number)(sample_rate)).doubleValue());
                audio = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(audio), java.util.stream.DoubleStream.of(sinApprox(2.0 * PI * 440.0 * t_1))).toArray()));
                n_5 = n_5 + 1;
            }
            coeffs = ((double[])(mfcc(((double[])(audio)), 5, 3)));
            for (double c : coeffs) {
                System.out.println(c);
            }
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
