public class Main {
    static double PI;
    static long sample_rate;
    static long size;
    static double[] audio = new double[0];
    static long n_10 = 0;
    static double[] coeffs;

    static double sinApprox(double x) {
        double term = x;
        double sum_1 = x;
        long n_1 = 1;
        while (n_1 <= 10) {
            double denom_1 = ((Number)(((2 * n_1) * (2 * n_1 + 1)))).doubleValue();
            term = -term * x * x / denom_1;
            sum_1 = sum_1 + term;
            n_1 = n_1 + 1;
        }
        return sum_1;
    }

    static double cosApprox(double x) {
        double term_1 = 1.0;
        double sum_3 = 1.0;
        long n_3 = 1;
        while (n_3 <= 10) {
            double denom_3 = ((Number)(((2 * n_3 - 1) * (2 * n_3)))).doubleValue();
            term_1 = -term_1 * x * x / denom_3;
            sum_3 = sum_3 + term_1;
            n_3 = n_3 + 1;
        }
        return sum_3;
    }

    static double expApprox(double x) {
        double sum_4 = 1.0;
        double term_3 = 1.0;
        long n_5 = 1;
        while (n_5 < 10) {
            term_3 = term_3 * x / (((Number)(n_5)).doubleValue());
            sum_4 = sum_4 + term_3;
            n_5 = n_5 + 1;
        }
        return sum_4;
    }

    static double ln(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term_5 = t;
        double sum_6 = 0.0;
        long n_7 = 1;
        while (n_7 <= 19) {
            sum_6 = sum_6 + term_5 / (((Number)(n_7)).doubleValue());
            term_5 = term_5 * t * t;
            n_7 = n_7 + 2;
        }
        return 2.0 * sum_6;
    }

    static double log10(double x) {
        return ln(x) / ln(10.0);
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_1 = 0;
        while (i_1 < 10) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double[] normalize(double[] audio) {
        double max_val = 0.0;
        long i_3 = 0;
        while (i_3 < audio.length) {
            double v_1 = absf(audio[(int)(i_3)]);
            if (v_1 > max_val) {
                max_val = v_1;
            }
            i_3 = i_3 + 1;
        }
        double[] res_1 = ((double[])(new double[]{}));
        i_3 = 0;
        while (i_3 < audio.length) {
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(audio[(int)(i_3)] / max_val)).toArray()));
            i_3 = i_3 + 1;
        }
        return res_1;
    }

    static double[] dft(double[] frame, long bins) {
        long N = frame.length;
        double[] spec_1 = ((double[])(new double[]{}));
        long k_1 = 0;
        while (k_1 < bins) {
            double real_1 = 0.0;
            double imag_1 = 0.0;
            long n_9 = 0;
            while (n_9 < N) {
                double angle_1 = -2.0 * PI * (((Number)(k_1)).doubleValue()) * (((Number)(n_9)).doubleValue()) / (((Number)(N)).doubleValue());
                real_1 = real_1 + frame[(int)(n_9)] * cosApprox(angle_1);
                imag_1 = imag_1 + frame[(int)(n_9)] * sinApprox(angle_1);
                n_9 = n_9 + 1;
            }
            spec_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(spec_1), java.util.stream.DoubleStream.of(real_1 * real_1 + imag_1 * imag_1)).toArray()));
            k_1 = k_1 + 1;
        }
        return spec_1;
    }

    static double[][] triangular_filters(long bins, long spectrum_size) {
        double[][] filters = ((double[][])(new double[][]{}));
        long b_1 = 0;
        while (b_1 < bins) {
            long center_1 = Math.floorDiv(((b_1 + 1) * spectrum_size), (bins + 1));
            double[] filt_1 = ((double[])(new double[]{}));
            long i_5 = 0;
            while (i_5 < spectrum_size) {
                double v_3 = 0.0;
                if (i_5 <= center_1) {
                    v_3 = (((Number)(i_5)).doubleValue()) / (((Number)(center_1)).doubleValue());
                } else {
                    v_3 = (((Number)((spectrum_size - i_5))).doubleValue()) / (((Number)((spectrum_size - center_1))).doubleValue());
                }
                filt_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(filt_1), java.util.stream.DoubleStream.of(v_3)).toArray()));
                i_5 = i_5 + 1;
            }
            filters = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(filters), java.util.stream.Stream.of(filt_1)).toArray(double[][]::new)));
            b_1 = b_1 + 1;
        }
        return filters;
    }

    static double[] dot(double[][] mat, double[] vec) {
        double[] res_2 = ((double[])(new double[]{}));
        long i_7 = 0;
        while (i_7 < mat.length) {
            double sum_8 = 0.0;
            long j_1 = 0;
            while (j_1 < vec.length) {
                sum_8 = sum_8 + mat[(int)(i_7)][(int)(j_1)] * vec[(int)(j_1)];
                j_1 = j_1 + 1;
            }
            res_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_2), java.util.stream.DoubleStream.of(sum_8)).toArray()));
            i_7 = i_7 + 1;
        }
        return res_2;
    }

    static double[][] discrete_cosine_transform(long dct_filter_num, long filter_num) {
        double[][] basis = ((double[][])(new double[][]{}));
        long i_9 = 0;
        while (i_9 < dct_filter_num) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_3 = 0;
            while (j_3 < filter_num) {
                if (i_9 == 0) {
                    row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(1.0 / sqrtApprox(((Number)(filter_num)).doubleValue()))).toArray()));
                } else {
                    double angle_3 = (((Number)((2 * j_3 + 1))).doubleValue()) * (((Number)(i_9)).doubleValue()) * PI / (2.0 * (((Number)(filter_num)).doubleValue()));
                    row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(cosApprox(angle_3) * sqrtApprox(2.0 / (((Number)(filter_num)).doubleValue())))).toArray()));
                }
                j_3 = j_3 + 1;
            }
            basis = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(basis), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            i_9 = i_9 + 1;
        }
        return basis;
    }

    static double[] mfcc(double[] audio, long bins, long dct_num) {
        double[] norm = ((double[])(normalize(((double[])(audio)))));
        double[] spec_3 = ((double[])(dft(((double[])(norm)), bins + 2)));
        double[][] filters_2 = ((double[][])(triangular_filters(bins, spec_3.length)));
        double[] energies_1 = ((double[])(dot(((double[][])(filters_2)), ((double[])(spec_3)))));
        double[] logfb_1 = ((double[])(new double[]{}));
        long i_11 = 0;
        while (i_11 < energies_1.length) {
            logfb_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(logfb_1), java.util.stream.DoubleStream.of(10.0 * log10(energies_1[(int)(i_11)] + 1e-10))).toArray()));
            i_11 = i_11 + 1;
        }
        double[][] dct_basis_1 = ((double[][])(discrete_cosine_transform(dct_num, bins)));
        double[] res_4 = ((double[])(dot(((double[][])(dct_basis_1)), ((double[])(logfb_1)))));
        if (res_4.length == 0) {
            res_4 = ((double[])(new double[]{0.0, 0.0, 0.0}));
        }
        return res_4;
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        sample_rate = 8000;
        size = 16;
        audio = ((double[])(new double[]{}));
        n_10 = 0;
        while (n_10 < size) {
            double t_1 = (((Number)(n_10)).doubleValue()) / (((Number)(sample_rate)).doubleValue());
            audio = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(audio), java.util.stream.DoubleStream.of(sinApprox(2.0 * PI * 440.0 * t_1))).toArray()));
            n_10 = n_10 + 1;
        }
        coeffs = ((double[])(mfcc(((double[])(audio)), 5, 3)));
        for (double c : coeffs) {
            System.out.println(c);
        }
    }
}
