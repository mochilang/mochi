public class Main {
    static class Polynomial {
        int degree;
        double[] coefficients;
        Polynomial(int degree, double[] coefficients) {
            this.degree = degree;
            this.coefficients = coefficients;
        }
        Polynomial() {}
        @Override public String toString() {
            return String.format("{'degree': %s, 'coefficients': %s}", String.valueOf(degree), String.valueOf(coefficients));
        }
    }


    static double[] copy_list(double[] xs) {
        double[] res = ((double[])(new double[]{}));
        int i = 0;
        while (i < xs.length) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        return res;
    }

    static Polynomial polynomial_new(int degree, double[] coeffs) {
        if (coeffs.length != degree + 1) {
            throw new RuntimeException(String.valueOf("The number of coefficients should be equal to the degree + 1."));
        }
        return new Polynomial(degree, copy_list(((double[])(coeffs))));
    }

    static Polynomial add(Polynomial p, Polynomial q) {
        if (p.degree > q.degree) {
            double[] coeffs = ((double[])(copy_list(((double[])(p.coefficients)))));
            int i_1 = 0;
            while (i_1 <= q.degree) {
coeffs[i_1] = coeffs[i_1] + q.coefficients[i_1];
                i_1 = i_1 + 1;
            }
            return new Polynomial(p.degree, coeffs);
        } else {
            double[] coeffs_1 = ((double[])(copy_list(((double[])(q.coefficients)))));
            int i_2 = 0;
            while (i_2 <= p.degree) {
coeffs_1[i_2] = coeffs_1[i_2] + p.coefficients[i_2];
                i_2 = i_2 + 1;
            }
            return new Polynomial(q.degree, coeffs_1);
        }
    }

    static Polynomial neg(Polynomial p) {
        double[] coeffs_2 = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 <= p.degree) {
            coeffs_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(coeffs_2), java.util.stream.DoubleStream.of(-p.coefficients[i_3])).toArray()));
            i_3 = i_3 + 1;
        }
        return new Polynomial(p.degree, coeffs_2);
    }

    static Polynomial sub(Polynomial p, Polynomial q) {
        return add(p, neg(q));
    }

    static Polynomial mul(Polynomial p, Polynomial q) {
        int size = p.degree + q.degree + 1;
        double[] coeffs_3 = ((double[])(new double[]{}));
        int i_4 = 0;
        while (i_4 < size) {
            coeffs_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(coeffs_3), java.util.stream.DoubleStream.of(0.0)).toArray()));
            i_4 = i_4 + 1;
        }
        i_4 = 0;
        while (i_4 <= p.degree) {
            int j = 0;
            while (j <= q.degree) {
coeffs_3[i_4 + j] = coeffs_3[i_4 + j] + p.coefficients[i_4] * q.coefficients[j];
                j = j + 1;
            }
            i_4 = i_4 + 1;
        }
        return new Polynomial(p.degree + q.degree, coeffs_3);
    }

    static double power(double base, int exp) {
        double result = 1.0;
        int i_5 = 0;
        while (i_5 < exp) {
            result = result * base;
            i_5 = i_5 + 1;
        }
        return result;
    }

    static double evaluate(Polynomial p, double x) {
        double result_1 = 0.0;
        int i_6 = 0;
        while (i_6 <= p.degree) {
            result_1 = result_1 + p.coefficients[i_6] * power(x, i_6);
            i_6 = i_6 + 1;
        }
        return result_1;
    }

    static String poly_to_string(Polynomial p) {
        String s = "";
        int i_7 = p.degree;
        while (i_7 >= 0) {
            double coeff = p.coefficients[i_7];
            if (coeff != 0.0) {
                if (_runeLen(s) > 0) {
                    if (coeff > 0.0) {
                        s = s + " + ";
                    } else {
                        s = s + " - ";
                    }
                } else                 if (coeff < 0.0) {
                    s = s + "-";
                }
                double abs_coeff = coeff < 0.0 ? -coeff : coeff;
                if (i_7 == 0) {
                    s = s + _p(abs_coeff);
                } else                 if (i_7 == 1) {
                    s = s + _p(abs_coeff) + "x";
                } else {
                    s = s + _p(abs_coeff) + "x^" + _p(i_7);
                }
            }
            i_7 = i_7 - 1;
        }
        if ((s.equals(""))) {
            s = "0";
        }
        return s;
    }

    static Polynomial derivative(Polynomial p) {
        if (p.degree == 0) {
            return new Polynomial(0, new double[]{0.0});
        }
        double[] coeffs_4 = ((double[])(new double[]{}));
        int i_8 = 0;
        while (i_8 < p.degree) {
            coeffs_4 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(coeffs_4), java.util.stream.DoubleStream.of(p.coefficients[i_8 + 1] * ((Number)(i_8 + 1)).doubleValue())).toArray()));
            i_8 = i_8 + 1;
        }
        return new Polynomial(p.degree - 1, coeffs_4);
    }

    static Polynomial integral(Polynomial p, double constant) {
        double[] coeffs_5 = ((double[])(new double[]{constant}));
        int i_9 = 0;
        while (i_9 <= p.degree) {
            coeffs_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(coeffs_5), java.util.stream.DoubleStream.of(p.coefficients[i_9] / ((Number)(i_9 + 1)).doubleValue())).toArray()));
            i_9 = i_9 + 1;
        }
        return new Polynomial(p.degree + 1, coeffs_5);
    }

    static boolean equals(Polynomial p, Polynomial q) {
        if (p.degree != q.degree) {
            return false;
        }
        int i_10 = 0;
        while (i_10 <= p.degree) {
            if (p.coefficients[i_10] != q.coefficients[i_10]) {
                return false;
            }
            i_10 = i_10 + 1;
        }
        return true;
    }

    static boolean not_equals(Polynomial p, Polynomial q) {
        return !(Boolean)equals(p, q);
    }

    static void test_polynomial() {
        Polynomial p = polynomial_new(2, ((double[])(new double[]{1.0, 2.0, 3.0})));
        Polynomial q = polynomial_new(2, ((double[])(new double[]{1.0, 2.0, 3.0})));
        if (!(poly_to_string(add(p, q)).equals("6x^2 + 4x + 2"))) {
            throw new RuntimeException(String.valueOf("add failed"));
        }
        if (!(poly_to_string(sub(p, q)).equals("0"))) {
            throw new RuntimeException(String.valueOf("sub failed"));
        }
        if (evaluate(p, 2.0) != 17.0) {
            throw new RuntimeException(String.valueOf("evaluate failed"));
        }
        if (!(poly_to_string(derivative(p)).equals("6x + 2"))) {
            throw new RuntimeException(String.valueOf("derivative failed"));
        }
        String integ = String.valueOf(poly_to_string(integral(p, 0.0)));
        if (!(integ.equals("1x^3 + 1x^2 + 1x"))) {
            throw new RuntimeException(String.valueOf("integral failed"));
        }
        if (!(Boolean)equals(p, q)) {
            throw new RuntimeException(String.valueOf("equals failed"));
        }
        if (((Boolean)(not_equals(p, q)))) {
            throw new RuntimeException(String.valueOf("not_equals failed"));
        }
    }

    static void main() {
        test_polynomial();
        Polynomial p_1 = polynomial_new(2, ((double[])(new double[]{1.0, 2.0, 3.0})));
        Polynomial d = derivative(p_1);
        System.out.println(poly_to_string(d));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
