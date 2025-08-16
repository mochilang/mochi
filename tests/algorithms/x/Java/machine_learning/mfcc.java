public class Main {
    static double PI = (double)(3.141592653589793);
    static long sample_rate = 8000L;
    static long size = 16L;
    static double[] audio = ((double[])(new double[]{}));
    static long n_10 = 0L;
    static double[] coeffs;

    static double sinApprox(double x) {
        double term = (double)(x);
        double sum_1 = (double)(x);
        long n_1 = 1L;
        while ((long)(n_1) <= 10L) {
            double denom_1 = (double)(((Number)(((long)((2L * (long)(n_1))) * (long)(((long)(2L * (long)(n_1)) + 1L))))).doubleValue());
            term = (double)((double)((double)((double)(-term) * (double)(x)) * (double)(x)) / (double)(denom_1));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return sum_1;
    }

    static double cosApprox(double x) {
        double term_1 = (double)(1.0);
        double sum_3 = (double)(1.0);
        long n_3 = 1L;
        while ((long)(n_3) <= 10L) {
            double denom_3 = (double)(((Number)(((long)(((long)(2L * (long)(n_3)) - 1L)) * (long)((2L * (long)(n_3)))))).doubleValue());
            term_1 = (double)((double)((double)((double)(-term_1) * (double)(x)) * (double)(x)) / (double)(denom_3));
            sum_3 = (double)((double)(sum_3) + (double)(term_1));
            n_3 = (long)((long)(n_3) + 1L);
        }
        return sum_3;
    }

    static double expApprox(double x) {
        double sum_4 = (double)(1.0);
        double term_3 = (double)(1.0);
        long n_5 = 1L;
        while ((long)(n_5) < 10L) {
            term_3 = (double)((double)((double)(term_3) * (double)(x)) / (double)((((Number)(n_5)).doubleValue())));
            sum_4 = (double)((double)(sum_4) + (double)(term_3));
            n_5 = (long)((long)(n_5) + 1L);
        }
        return sum_4;
    }

    static double ln(double x) {
        double t = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double term_5 = (double)(t);
        double sum_6 = (double)(0.0);
        long n_7 = 1L;
        while ((long)(n_7) <= 19L) {
            sum_6 = (double)((double)(sum_6) + (double)((double)(term_5) / (double)((((Number)(n_7)).doubleValue()))));
            term_5 = (double)((double)((double)(term_5) * (double)(t)) * (double)(t));
            n_7 = (long)((long)(n_7) + 2L);
        }
        return (double)(2.0) * (double)(sum_6);
    }

    static double log10(double x) {
        return (double)(ln((double)(x))) / (double)(ln((double)(10.0)));
    }

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 10L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double[] normalize(double[] audio) {
        double max_val = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(audio.length)) {
            double v_1 = (double)(absf((double)(audio[(int)((long)(i_3))])));
            if ((double)(v_1) > (double)(max_val)) {
                max_val = (double)(v_1);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        double[] res_1 = ((double[])(new double[]{}));
        i_3 = 0L;
        while ((long)(i_3) < (long)(audio.length)) {
            res_1 = ((double[])(appendDouble(res_1, (double)((double)(audio[(int)((long)(i_3))]) / (double)(max_val)))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res_1;
    }

    static double[] dft(double[] frame, long bins) {
        long N = (long)(frame.length);
        double[] spec_1 = ((double[])(new double[]{}));
        long k_1 = 0L;
        while ((long)(k_1) < (long)(bins)) {
            double real_1 = (double)(0.0);
            double imag_1 = (double)(0.0);
            long n_9 = 0L;
            while ((long)(n_9) < (long)(N)) {
                double angle_1 = (double)((double)((double)((double)((double)(-2.0) * (double)(PI)) * (double)((((Number)(k_1)).doubleValue()))) * (double)((((Number)(n_9)).doubleValue()))) / (double)((((Number)(N)).doubleValue())));
                real_1 = (double)((double)(real_1) + (double)((double)(frame[(int)((long)(n_9))]) * (double)(cosApprox((double)(angle_1)))));
                imag_1 = (double)((double)(imag_1) + (double)((double)(frame[(int)((long)(n_9))]) * (double)(sinApprox((double)(angle_1)))));
                n_9 = (long)((long)(n_9) + 1L);
            }
            spec_1 = ((double[])(appendDouble(spec_1, (double)((double)((double)(real_1) * (double)(real_1)) + (double)((double)(imag_1) * (double)(imag_1))))));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return spec_1;
    }

    static double[][] triangular_filters(long bins, long spectrum_size) {
        double[][] filters = ((double[][])(new double[][]{}));
        long b_1 = 0L;
        while ((long)(b_1) < (long)(bins)) {
            long center_1 = Math.floorDiv(((long)(((long)(((long)(b_1) + 1L)) * (long)(spectrum_size)))), ((long)(((long)(bins) + 1L))));
            double[] filt_1 = ((double[])(new double[]{}));
            long i_5 = 0L;
            while ((long)(i_5) < (long)(spectrum_size)) {
                double v_3 = (double)(0.0);
                if ((long)(i_5) <= (long)(center_1)) {
                    v_3 = (double)((double)((((Number)(i_5)).doubleValue())) / (double)((((Number)(center_1)).doubleValue())));
                } else {
                    v_3 = (double)((double)((((Number)(((long)(spectrum_size) - (long)(i_5)))).doubleValue())) / (double)((((Number)(((long)(spectrum_size) - (long)(center_1)))).doubleValue())));
                }
                filt_1 = ((double[])(appendDouble(filt_1, (double)(v_3))));
                i_5 = (long)((long)(i_5) + 1L);
            }
            filters = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(filters), java.util.stream.Stream.of(new double[][]{filt_1})).toArray(double[][]::new)));
            b_1 = (long)((long)(b_1) + 1L);
        }
        return filters;
    }

    static double[] dot(double[][] mat, double[] vec) {
        double[] res_2 = ((double[])(new double[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(mat.length)) {
            double sum_8 = (double)(0.0);
            long j_1 = 0L;
            while ((long)(j_1) < (long)(vec.length)) {
                sum_8 = (double)((double)(sum_8) + (double)((double)(mat[(int)((long)(i_7))][(int)((long)(j_1))]) * (double)(vec[(int)((long)(j_1))])));
                j_1 = (long)((long)(j_1) + 1L);
            }
            res_2 = ((double[])(appendDouble(res_2, (double)(sum_8))));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return res_2;
    }

    static double[][] discrete_cosine_transform(long dct_filter_num, long filter_num) {
        double[][] basis = ((double[][])(new double[][]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(dct_filter_num)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(filter_num)) {
                if ((long)(i_9) == 0L) {
                    row_1 = ((double[])(appendDouble(row_1, (double)((double)(1.0) / (double)(sqrtApprox((double)(((Number)(filter_num)).doubleValue())))))));
                } else {
                    double angle_3 = (double)((double)((double)((double)((((Number)(((long)(2L * (long)(j_3)) + 1L))).doubleValue())) * (double)((((Number)(i_9)).doubleValue()))) * (double)(PI)) / (double)(((double)(2.0) * (double)((((Number)(filter_num)).doubleValue())))));
                    row_1 = ((double[])(appendDouble(row_1, (double)((double)(cosApprox((double)(angle_3))) * (double)(sqrtApprox((double)((double)(2.0) / (double)((((Number)(filter_num)).doubleValue())))))))));
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            basis = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(basis), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return basis;
    }

    static double[] mfcc(double[] audio, long bins, long dct_num) {
        double[] norm = ((double[])(normalize(((double[])(audio)))));
        double[] spec_3 = ((double[])(dft(((double[])(norm)), (long)((long)(bins) + 2L))));
        double[][] filters_2 = ((double[][])(triangular_filters((long)(bins), (long)(spec_3.length))));
        double[] energies_1 = ((double[])(dot(((double[][])(filters_2)), ((double[])(spec_3)))));
        double[] logfb_1 = ((double[])(new double[]{}));
        long i_11 = 0L;
        while ((long)(i_11) < (long)(energies_1.length)) {
            logfb_1 = ((double[])(appendDouble(logfb_1, (double)((double)(10.0) * (double)(log10((double)((double)(energies_1[(int)((long)(i_11))]) + (double)(1e-10))))))));
            i_11 = (long)((long)(i_11) + 1L);
        }
        double[][] dct_basis_1 = ((double[][])(discrete_cosine_transform((long)(dct_num), (long)(bins))));
        double[] res_4 = ((double[])(dot(((double[][])(dct_basis_1)), ((double[])(logfb_1)))));
        if ((long)(res_4.length) == 0L) {
            res_4 = ((double[])(new double[]{0.0, 0.0, 0.0}));
        }
        return res_4;
    }
    public static void main(String[] args) {
        while ((long)(n_10) < (long)(size)) {
            double t_1 = (double)((double)((((Number)(n_10)).doubleValue())) / (double)((((Number)(sample_rate)).doubleValue())));
            audio = ((double[])(appendDouble(audio, (double)(sinApprox((double)((double)((double)((double)(2.0) * (double)(PI)) * (double)(440.0)) * (double)(t_1)))))));
            n_10 = (long)((long)(n_10) + 1L);
        }
        coeffs = ((double[])(mfcc(((double[])(audio)), 5L, 3L)));
        for (double c : coeffs) {
            System.out.println(c);
        }
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
