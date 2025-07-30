public class Main {

    static double pow10(int n) {
        double r = 1.0;
        int i = 0;
        while (i < n) {
            r = r * 10.0;
            i = i + 1;
        }
        return r;
    }

    static double powf(double base, double exp) {
        if (exp == 0.5) {
            double guess = base;
            int i = 0;
            while (i < 20) {
                guess = (guess + base / guess) / 2.0;
                i = i + 1;
            }
            return guess;
        }
        double result = 1.0;
        int n = ((Number)(exp)).intValue();
        int i = 0;
        while (i < n) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static String formatFloat(double f, int prec) {
        double scale = pow10(prec);
        double scaled = (f * scale) + 0.5;
        int n = (((Number)(scaled)).intValue());
        String digits = String.valueOf(n);
        while (_runeLen(digits) <= prec) {
            digits = "0" + digits;
        }
        String intPart = _substr(digits, 0, _runeLen(digits) - prec);
        String fracPart = _substr(digits, _runeLen(digits) - prec, _runeLen(digits));
        return intPart + "." + fracPart;
    }

    static String padLeft(String s, int w) {
        String res = "";
        int n = w - _runeLen(s);
        while (n > 0) {
            res = res + " ";
            n = n - 1;
        }
        return res + s;
    }

    static String rowString(double[] row) {
        String s = "[";
        int i = 0;
        while (i < row.length) {
            s = s + String.valueOf(padLeft(String.valueOf(formatFloat(row[i], 3)), 6));
            if (i < row.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        return s + "] ";
    }

    static void printMatrix(String heading, double[][] m) {
        System.out.println(heading);
        int i = 0;
        while (i < m.length) {
            System.out.println(rowString(m[i]));
            i = i + 1;
        }
    }

    static double[][] elementWiseMM(double[][] m1, double[][] m2, java.util.function.BiFunction<Double,Double,Double> f) {
        double[][] z = new double[][]{};
        int r = 0;
        while (r < m1.length) {
            double[] row = new double[]{};
            int c = 0;
            while (c < m1[r].length) {
                row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(f.apply(m1[r][c], m2[r][c]))).toArray();
                c = c + 1;
            }
            z = appendObj(z, row);
            r = r + 1;
        }
        return z;
    }

    static double[][] elementWiseMS(double[][] m, double s, java.util.function.BiFunction<Double,Double,Double> f) {
        double[][] z = new double[][]{};
        int r = 0;
        while (r < m.length) {
            double[] row = new double[]{};
            int c = 0;
            while (c < m[r].length) {
                row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(f.apply(m[r][c], s))).toArray();
                c = c + 1;
            }
            z = appendObj(z, row);
            r = r + 1;
        }
        return z;
    }

    static double add(double a, double b) {
        return a + b;
    }

    static double sub(double a, double b) {
        return a - b;
    }

    static double mul(double a, double b) {
        return a * b;
    }

    static double div(double a, double b) {
        return a / b;
    }

    static double exp(double a, double b) {
        return powf(a, b);
    }

    static void main() {
        double[][] m1 = new double[][]{new double[]{3.0, 1.0, 4.0}, new double[]{1.0, 5.0, 9.0}};
        double[][] m2 = new double[][]{new double[]{2.0, 7.0, 1.0}, new double[]{8.0, 2.0, 8.0}};
        printMatrix("m1:", m1);
        printMatrix("m2:", m2);
        System.out.println("");
        printMatrix("m1 + m2:", elementWiseMM(m1, m2, Main::add));
        printMatrix("m1 - m2:", elementWiseMM(m1, m2, Main::sub));
        printMatrix("m1 * m2:", elementWiseMM(m1, m2, Main::mul));
        printMatrix("m1 / m2:", elementWiseMM(m1, m2, Main::div));
        printMatrix("m1 ^ m2:", elementWiseMM(m1, m2, Main::exp));
        System.out.println("");
        double s = 0.5;
        System.out.println("s: " + String.valueOf(s));
        printMatrix("m1 + s:", elementWiseMS(m1, s, Main::add));
        printMatrix("m1 - s:", elementWiseMS(m1, s, Main::sub));
        printMatrix("m1 * s:", elementWiseMS(m1, s, Main::mul));
        printMatrix("m1 / s:", elementWiseMS(m1, s, Main::div));
        printMatrix("m1 ^ s:", elementWiseMS(m1, s, Main::exp));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
