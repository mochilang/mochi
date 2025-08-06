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


    static Complex complex_add(Complex a, Complex b) {
        return new Complex(a.re + b.re, a.im + b.im);
    }

    static Complex complex_mul(Complex a, Complex b) {
        double real = a.re * b.re - a.im * b.im;
        double imag = a.re * b.im + a.im * b.re;
        return new Complex(real, imag);
    }

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double complex_abs(Complex a) {
        return sqrtApprox(a.re * a.re + a.im * a.im);
    }

    static double sin_taylor(double x) {
        double term = x;
        double sum = x;
        int i_1 = 1;
        while (i_1 < 10) {
            double k1 = 2.0 * (((Number)(i_1)).doubleValue());
            double k2 = 2.0 * (((Number)(i_1)).doubleValue()) + 1.0;
            term = -term * x * x / (k1 * k2);
            sum = sum + term;
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static double cos_taylor(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int i_2 = 1;
        while (i_2 < 10) {
            double k1_1 = 2.0 * (((Number)(i_2)).doubleValue()) - 1.0;
            double k2_1 = 2.0 * (((Number)(i_2)).doubleValue());
            term_1 = -term_1 * x * x / (k1_1 * k2_1);
            sum_1 = sum_1 + term_1;
            i_2 = i_2 + 1;
        }
        return sum_1;
    }

    static double exp_taylor(double x) {
        double term_2 = 1.0;
        double sum_2 = 1.0;
        double i_3 = 1.0;
        while (i_3 < 20.0) {
            term_2 = term_2 * x / i_3;
            sum_2 = sum_2 + term_2;
            i_3 = i_3 + 1.0;
        }
        return sum_2;
    }

    static Complex complex_exp(Complex z) {
        double e = exp_taylor(z.re);
        return new Complex(e * cos_taylor(z.im), e * sin_taylor(z.im));
    }

    static Complex eval_quadratic(Complex c, Complex z) {
        return complex_add(complex_mul(z, z), c);
    }

    static Complex eval_exponential(Complex c, Complex z) {
        return complex_add(complex_exp(z), c);
    }

    static Complex iterate_function(java.util.function.BiFunction<Complex,Complex,Complex> eval_function, Complex c, int nb_iterations, Complex z0, double infinity) {
        Complex z_n = z0;
        int i_4 = 0;
        while (i_4 < nb_iterations) {
            z_n = eval_function.apply(c, z_n);
            if (complex_abs(z_n) > infinity) {
                return z_n;
            }
            i_4 = i_4 + 1;
        }
        return z_n;
    }

    static Complex[][] prepare_grid(double window_size, int nb_pixels) {
        Complex[][] grid = ((Complex[][])(new Complex[][]{}));
        int i_5 = 0;
        while (i_5 < nb_pixels) {
            Complex[] row = ((Complex[])(new Complex[]{}));
            int j = 0;
            while (j < nb_pixels) {
                double real_1 = -window_size + 2.0 * window_size * (((Number)(i_5)).doubleValue()) / (((Number)((nb_pixels - 1))).doubleValue());
                double imag_1 = -window_size + 2.0 * window_size * (((Number)(j)).doubleValue()) / (((Number)((nb_pixels - 1))).doubleValue());
                row = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(new Complex(real_1, imag_1))).toArray(Complex[]::new)));
                j = j + 1;
            }
            grid = ((Complex[][])(appendObj(grid, row)));
            i_5 = i_5 + 1;
        }
        return grid;
    }

    static void julia_demo() {
        Complex[][] grid_1 = ((Complex[][])(prepare_grid(1.0, 5)));
        Complex c_poly = new Complex(-0.4, 0.6);
        Complex c_exp = new Complex(-2.0, 0.0);
        int[][] poly_result = ((int[][])(new int[][]{}));
        int[][] exp_result = ((int[][])(new int[][]{}));
        int y = 0;
        while (y < grid_1.length) {
            int[] row_poly = ((int[])(new int[]{}));
            int[] row_exp = ((int[])(new int[]{}));
            int x = 0;
            while (x < grid_1[y].length) {
                Complex z0 = grid_1[y][x];
                Complex z_poly = iterate_function(Main::eval_quadratic, c_poly, 20, z0, 4.0);
                Complex z_exp = iterate_function(Main::eval_exponential, c_exp, 10, z0, 10000000000.0);
                row_poly = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_poly), java.util.stream.IntStream.of(complex_abs(z_poly) < 2.0 ? 1 : 0)).toArray()));
                row_exp = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_exp), java.util.stream.IntStream.of(complex_abs(z_exp) < 10000.0 ? 1 : 0)).toArray()));
                x = x + 1;
            }
            poly_result = ((int[][])(appendObj(poly_result, row_poly)));
            exp_result = ((int[][])(appendObj(exp_result, row_exp)));
            y = y + 1;
        }
        System.out.println(java.util.Arrays.deepToString(poly_result));
        System.out.println(java.util.Arrays.deepToString(exp_result));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            julia_demo();
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
