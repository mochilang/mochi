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
        double imag = a.re * b.im + a.im * b.re;
        return new Complex(real, imag);
    }

    static Complex[] conj_vector(Complex[] v) {
        Complex[] res = ((Complex[])(new Complex[]{}));
        int i = 0;
        while (i < v.length) {
            res = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(complex_conj(v[i]))).toArray(Complex[]::new)));
            i = i + 1;
        }
        return res;
    }

    static Complex[] vec_mat_mul(Complex[] v, Complex[][] m) {
        Complex[] result = ((Complex[])(new Complex[]{}));
        int col = 0;
        while (col < m[0].length) {
            Complex sum = new Complex(0.0, 0.0);
            int row = 0;
            while (row < v.length) {
                sum = complex_add(sum, complex_mul(v[row], m[row][col]));
                row = row + 1;
            }
            result = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(sum)).toArray(Complex[]::new)));
            col = col + 1;
        }
        return result;
    }

    static Complex dot(Complex[] a, Complex[] b) {
        Complex sum_1 = new Complex(0.0, 0.0);
        int i_1 = 0;
        while (i_1 < a.length) {
            sum_1 = complex_add(sum_1, complex_mul(a[i_1], b[i_1]));
            i_1 = i_1 + 1;
        }
        return sum_1;
    }

    static boolean is_hermitian(Complex[][] m) {
        int i_2 = 0;
        while (i_2 < m.length) {
            int j = 0;
            while (j < m.length) {
                if (!(Boolean)complex_eq(m[i_2][j], complex_conj(m[j][i_2]))) {
                    return false;
                }
                j = j + 1;
            }
            i_2 = i_2 + 1;
        }
        return true;
    }

    static double rayleigh_quotient(Complex[][] a, Complex[] v) {
        Complex[] v_star = ((Complex[])(conj_vector(((Complex[])(v)))));
        Complex[] v_star_dot = ((Complex[])(vec_mat_mul(((Complex[])(v_star)), ((Complex[][])(a)))));
        Complex num = dot(((Complex[])(v_star_dot)), ((Complex[])(v)));
        Complex den = dot(((Complex[])(v_star)), ((Complex[])(v)));
        return num.re / den.re;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
}
