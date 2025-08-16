public class Main {
    static class Dual {
        double real;
        double[] duals;
        Dual(double real, double[] duals) {
            this.real = real;
            this.duals = duals;
        }
        Dual() {}
        @Override public String toString() {
            return String.format("{'real': %s, 'duals': %s}", String.valueOf(real), String.valueOf(duals));
        }
    }


    static Dual make_dual(double real, long rank) {
        double[] ds = ((double[])(new double[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(rank)) {
            ds = ((double[])(appendDouble(ds, (double)(1.0))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return new Dual(real, ds);
    }

    static Dual dual_from_list(double real, double[] ds) {
        return new Dual(real, ds);
    }

    static Dual dual_add(Dual a, Dual b) {
        double[] s_dual = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(a.duals.length)) {
            s_dual = ((double[])(appendDouble(s_dual, (double)(a.duals[(int)((long)(i_3))]))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        double[] o_dual_1 = ((double[])(new double[]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(b.duals.length)) {
            o_dual_1 = ((double[])(appendDouble(o_dual_1, (double)(b.duals[(int)((long)(j_1))]))));
            j_1 = (long)((long)(j_1) + 1L);
        }
        if ((long)(s_dual.length) > (long)(o_dual_1.length)) {
            long diff_1 = (long)((long)(s_dual.length) - (long)(o_dual_1.length));
            long k_1 = 0L;
            while ((long)(k_1) < (long)(diff_1)) {
                o_dual_1 = ((double[])(appendDouble(o_dual_1, (double)(1.0))));
                k_1 = (long)((long)(k_1) + 1L);
            }
        } else         if ((long)(s_dual.length) < (long)(o_dual_1.length)) {
            long diff2_1 = (long)((long)(o_dual_1.length) - (long)(s_dual.length));
            long k2_1 = 0L;
            while ((long)(k2_1) < (long)(diff2_1)) {
                s_dual = ((double[])(appendDouble(s_dual, (double)(1.0))));
                k2_1 = (long)((long)(k2_1) + 1L);
            }
        }
        double[] new_duals_1 = ((double[])(new double[]{}));
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(s_dual.length)) {
            new_duals_1 = ((double[])(appendDouble(new_duals_1, (double)((double)(s_dual[(int)((long)(idx_1))]) + (double)(o_dual_1[(int)((long)(idx_1))])))));
            idx_1 = (long)((long)(idx_1) + 1L);
        }
        return new Dual((double)(a.real) + (double)(b.real), new_duals_1);
    }

    static Dual dual_add_real(Dual a, double b) {
        double[] ds_1 = ((double[])(new double[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(a.duals.length)) {
            ds_1 = ((double[])(appendDouble(ds_1, (double)(a.duals[(int)((long)(i_5))]))));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return new Dual((double)(a.real) + (double)(b), ds_1);
    }

    static Dual dual_mul(Dual a, Dual b) {
        long new_len = (long)((long)((long)(a.duals.length) + (long)(b.duals.length)) + 1L);
        double[] new_duals_3 = ((double[])(new double[]{}));
        long idx_3 = 0L;
        while ((long)(idx_3) < (long)(new_len)) {
            new_duals_3 = ((double[])(appendDouble(new_duals_3, (double)(0.0))));
            idx_3 = (long)((long)(idx_3) + 1L);
        }
        long i_7 = 0L;
        while ((long)(i_7) < (long)(a.duals.length)) {
            long j_3 = 0L;
            while ((long)(j_3) < (long)(b.duals.length)) {
                long pos_1 = (long)((long)((long)(i_7) + (long)(j_3)) + 1L);
                double val_1 = (double)((double)(new_duals_3[(int)((long)(pos_1))]) + (double)((double)(a.duals[(int)((long)(i_7))]) * (double)(b.duals[(int)((long)(j_3))])));
new_duals_3[(int)((long)(pos_1))] = (double)(val_1);
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        long k_3 = 0L;
        while ((long)(k_3) < (long)(a.duals.length)) {
            double val_3 = (double)((double)(new_duals_3[(int)((long)(k_3))]) + (double)((double)(a.duals[(int)((long)(k_3))]) * (double)(b.real)));
new_duals_3[(int)((long)(k_3))] = (double)(val_3);
            k_3 = (long)((long)(k_3) + 1L);
        }
        long l_1 = 0L;
        while ((long)(l_1) < (long)(b.duals.length)) {
            double val_5 = (double)((double)(new_duals_3[(int)((long)(l_1))]) + (double)((double)(b.duals[(int)((long)(l_1))]) * (double)(a.real)));
new_duals_3[(int)((long)(l_1))] = (double)(val_5);
            l_1 = (long)((long)(l_1) + 1L);
        }
        return new Dual((double)(a.real) * (double)(b.real), new_duals_3);
    }

    static Dual dual_mul_real(Dual a, double b) {
        double[] ds_2 = ((double[])(new double[]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(a.duals.length)) {
            ds_2 = ((double[])(appendDouble(ds_2, (double)((double)(a.duals[(int)((long)(i_9))]) * (double)(b)))));
            i_9 = (long)((long)(i_9) + 1L);
        }
        return new Dual((double)(a.real) * (double)(b), ds_2);
    }

    static Dual dual_pow(Dual x, long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("power must be a positive integer"));
        }
        if ((long)(n) == 0L) {
            return new Dual(1.0, new double[]{});
        }
        Dual res_1 = x;
        long i_11 = 1L;
        while ((long)(i_11) < (long)(n)) {
            res_1 = dual_mul(res_1, x);
            i_11 = (long)((long)(i_11) + 1L);
        }
        return res_1;
    }

    static double factorial(long n) {
        double res_2 = (double)(1.0);
        long i_13 = 2L;
        while ((long)(i_13) <= (long)(n)) {
            res_2 = (double)((double)(res_2) * (double)((((Number)(i_13)).doubleValue())));
            i_13 = (long)((long)(i_13) + 1L);
        }
        return res_2;
    }

    static double differentiate(java.util.function.Function<Dual,Dual> func, double position, long order) {
        Dual d = make_dual((double)(position), 1L);
        Dual result_1 = func.apply(d);
        if ((long)(order) == 0L) {
            return result_1.real;
        }
        return (double)(result_1.duals[(int)((long)((long)(order) - 1L))]) * (double)(factorial((long)(order)));
    }

    static void test_differentiate() {
        java.util.function.Function<Dual,Dual>[] f1 = new java.util.function.Function[1];
        f1[0] = (x) -> dual_pow(x, 2L);
        if ((double)(differentiate(f1[0], (double)(2.0), 2L)) != (double)(2.0)) {
            throw new RuntimeException(String.valueOf("f1 failed"));
        }
        java.util.function.Function<Dual,Dual>[] f2 = new java.util.function.Function[1];
        f2[0] = (x) -> dual_mul(dual_pow(x, 2L), dual_pow(x, 4L));
        if ((double)(differentiate(f2[0], (double)(9.0), 2L)) != (double)(196830.0)) {
            throw new RuntimeException(String.valueOf("f2 failed"));
        }
        java.util.function.Function<Dual,Dual>[] f3 = new java.util.function.Function[1];
        f3[0] = (y) -> dual_mul_real(dual_pow(dual_add_real(y, (double)(3.0)), 6L), (double)(0.5));
        if ((double)(differentiate(f3[0], (double)(3.5), 4L)) != (double)(7605.0)) {
            throw new RuntimeException(String.valueOf("f3 failed"));
        }
        java.util.function.Function<Dual,Dual>[] f4 = new java.util.function.Function[1];
        f4[0] = (y) -> dual_pow(y, 2L);
        if ((double)(differentiate(f4[0], (double)(4.0), 3L)) != (double)(0.0)) {
            throw new RuntimeException(String.valueOf("f4 failed"));
        }
    }

    static void main() {
        test_differentiate();
        java.util.function.Function<Dual,Dual>[] f = new java.util.function.Function[1];
        f[0] = (y) -> dual_mul(dual_pow(y, 2L), dual_pow(y, 4L));
        double res_4 = (double)(differentiate(f[0], (double)(9.0), 2L));
        System.out.println(res_4);
    }
    public static void main(String[] args) {
        main();
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
