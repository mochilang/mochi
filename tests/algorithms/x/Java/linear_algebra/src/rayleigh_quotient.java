public class Main {
    static class Complex {
        double re;
        double im;
        Complex(double re, double im) {
            this.re = re;
            this.im = im;
        }
        Complex() {}
        @Override public String toString() {
            return String.format("{'re': %s, 'im': %s}", String.valueOf(re), String.valueOf(im));
        }
    }

    static Complex[][] a;
    static Complex[] v;
    static Complex[][] b;

    static Complex complex_conj(Complex z) {
        return new Complex(z.re, -z.im);
    }

    static boolean complex_eq(Complex a, Complex b) {
        return a.re == b.re && a.im == b.im;
    }

    static Complex complex_add(Complex a, Complex b) {
        return new Complex(a.re + b.re, a.im + b.im);
    }

    static Complex complex_mul(Complex a, Complex b) {
        double real = a.re * b.re - a.im * b.im;
        double imag_1 = a.re * b.im + a.im * b.re;
        return new Complex(real, imag_1);
    }

    static Complex[] conj_vector(Complex[] v) {
        Complex[] res = ((Complex[])(new Complex[]{}));
        long i_1 = 0L;
        while (i_1 < v.length) {
            res = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(complex_conj(v[(int)((long)(i_1))]))).toArray(Complex[]::new)));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static Complex[] vec_mat_mul(Complex[] v, Complex[][] m) {
        Complex[] result = ((Complex[])(new Complex[]{}));
        long col_1 = 0L;
        while (col_1 < m[(int)((long)(0))].length) {
            Complex sum_1 = new Complex(0.0, 0.0);
            long row_1 = 0L;
            while (row_1 < v.length) {
                sum_1 = complex_add(sum_1, complex_mul(v[(int)((long)(row_1))], m[(int)((long)(row_1))][(int)((long)(col_1))]));
                row_1 = row_1 + 1;
            }
            result = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(sum_1)).toArray(Complex[]::new)));
            col_1 = col_1 + 1;
        }
        return result;
    }

    static Complex dot(Complex[] a, Complex[] b) {
        Complex sum_2 = new Complex(0.0, 0.0);
        long i_3 = 0L;
        while (i_3 < a.length) {
            sum_2 = complex_add(sum_2, complex_mul(a[(int)((long)(i_3))], b[(int)((long)(i_3))]));
            i_3 = i_3 + 1;
        }
        return sum_2;
    }

    static boolean is_hermitian(Complex[][] m) {
        long i_4 = 0L;
        while (i_4 < m.length) {
            long j_1 = 0L;
            while (j_1 < m.length) {
                if (!(Boolean)complex_eq(m[(int)((long)(i_4))][(int)((long)(j_1))], complex_conj(m[(int)((long)(j_1))][(int)((long)(i_4))]))) {
                    return false;
                }
                j_1 = j_1 + 1;
            }
            i_4 = i_4 + 1;
        }
        return true;
    }

    static double rayleigh_quotient(Complex[][] a, Complex[] v) {
        Complex[] v_star = ((Complex[])(conj_vector(((Complex[])(v)))));
        Complex[] v_star_dot_1 = ((Complex[])(vec_mat_mul(((Complex[])(v_star)), ((Complex[][])(a)))));
        Complex num_1 = dot(((Complex[])(v_star_dot_1)), ((Complex[])(v)));
        Complex den_1 = dot(((Complex[])(v_star)), ((Complex[])(v)));
        return num_1.re / den_1.re;
    }
    public static void main(String[] args) {
        a = ((Complex[][])(new Complex[][]{new Complex[]{new Complex(2.0, 0.0), new Complex(2.0, 1.0), new Complex(4.0, 0.0)}, new Complex[]{new Complex(2.0, -1.0), new Complex(3.0, 0.0), new Complex(0.0, 1.0)}, new Complex[]{new Complex(4.0, 0.0), new Complex(0.0, -1.0), new Complex(1.0, 0.0)}}));
        v = ((Complex[])(new Complex[]{new Complex(1.0, 0.0), new Complex(2.0, 0.0), new Complex(3.0, 0.0)}));
        if (((Boolean)(is_hermitian(((Complex[][])(a)))))) {
            double r1 = rayleigh_quotient(((Complex[][])(a)), ((Complex[])(v)));
            System.out.println(r1);
            System.out.println("\n");
        }
        b = ((Complex[][])(new Complex[][]{new Complex[]{new Complex(1.0, 0.0), new Complex(2.0, 0.0), new Complex(4.0, 0.0)}, new Complex[]{new Complex(2.0, 0.0), new Complex(3.0, 0.0), new Complex(-1.0, 0.0)}, new Complex[]{new Complex(4.0, 0.0), new Complex(-1.0, 0.0), new Complex(1.0, 0.0)}}));
        if (((Boolean)(is_hermitian(((Complex[][])(b)))))) {
            double r2 = rayleigh_quotient(((Complex[][])(b)), ((Complex[])(v)));
            System.out.println(r2);
        }
    }
}
