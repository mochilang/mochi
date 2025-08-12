public class Main {

    static boolean contains_int(long[] xs, long x) {
        long i = 0L;
        while (i < xs.length) {
            if (xs[(int)((long)(i))] == x) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String[] split(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        String current_1 = "";
        long i_2 = 0L;
        while (i_2 < _runeLen(s)) {
            String ch_1 = _substr(s, (int)((long)(i_2)), (int)((long)(i_2 + 1))));
            if ((ch_1.equals(sep))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else {
                current_1 = current_1 + ch_1;
            }
            i_2 = i_2 + 1;
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        return res;
    }

    static double pow_int_float(long base, long exp) {
        double result = 1.0;
        long i_4 = 0L;
        while (i_4 < exp) {
            result = result * (((Number)(base)).doubleValue());
            i_4 = i_4 + 1;
        }
        return result;
    }

    static String points_to_polynomial(long[][] coordinates) {
        if (coordinates.length == 0) {
            throw new RuntimeException(String.valueOf("The program cannot work out a fitting polynomial."));
        }
        long i_6 = 0L;
        while (i_6 < coordinates.length) {
            if (coordinates[(int)((long)(i_6))].length != 2) {
                throw new RuntimeException(String.valueOf("The program cannot work out a fitting polynomial."));
            }
            i_6 = i_6 + 1;
        }
        long j_1 = 0L;
        while (j_1 < coordinates.length) {
            long k_1 = j_1 + 1;
            while (k_1 < coordinates.length) {
                if (coordinates[(int)((long)(j_1))][(int)((long)(0))] == coordinates[(int)((long)(k_1))][(int)((long)(0))] && coordinates[(int)((long)(j_1))][(int)((long)(1))] == coordinates[(int)((long)(k_1))][(int)((long)(1))]) {
                    throw new RuntimeException(String.valueOf("The program cannot work out a fitting polynomial."));
                }
                k_1 = k_1 + 1;
            }
            j_1 = j_1 + 1;
        }
        long[] set_x_1 = ((long[])(new long[]{}));
        i_6 = 0;
        while (i_6 < coordinates.length) {
            long x_val_1 = coordinates[(int)((long)(i_6))][(int)((long)(0))];
            if (!(Boolean)contains_int(((long[])(set_x_1)), x_val_1)) {
                set_x_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(set_x_1), java.util.stream.LongStream.of(x_val_1)).toArray()));
            }
            i_6 = i_6 + 1;
        }
        if (set_x_1.length == 1) {
            return "x=" + _p(_geti(coordinates[(int)((long)(0))], ((Number)(0)).intValue()));
        }
        if (set_x_1.length != coordinates.length) {
            throw new RuntimeException(String.valueOf("The program cannot work out a fitting polynomial."));
        }
        long n_1 = coordinates.length;
        double[][] matrix_1 = ((double[][])(new double[][]{}));
        long row_1 = 0L;
        while (row_1 < n_1) {
            double[] line_1 = ((double[])(new double[]{}));
            long col_1 = 0L;
            while (col_1 < n_1) {
                double power_1 = pow_int_float(coordinates[(int)((long)(row_1))][(int)((long)(0))], n_1 - (col_1 + 1));
                line_1 = ((double[])(appendDouble(line_1, power_1)));
                col_1 = col_1 + 1;
            }
            matrix_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix_1), java.util.stream.Stream.of(line_1)).toArray(double[][]::new)));
            row_1 = row_1 + 1;
        }
        double[] vector_1 = ((double[])(new double[]{}));
        row_1 = 0;
        while (row_1 < n_1) {
            vector_1 = ((double[])(appendDouble(vector_1, ((double)(coordinates[(int)((long)(row_1))][(int)((long)(1))])))));
            row_1 = row_1 + 1;
        }
        long count_1 = 0L;
        while (count_1 < n_1) {
            long number_1 = 0L;
            while (number_1 < n_1) {
                if (count_1 != number_1) {
                    double fraction_1 = matrix_1[(int)((long)(number_1))][(int)((long)(count_1))] / matrix_1[(int)((long)(count_1))][(int)((long)(count_1))];
                    long cc_1 = 0L;
                    while (cc_1 < n_1) {
matrix_1[(int)((long)(number_1))][(int)((long)(cc_1))] = matrix_1[(int)((long)(number_1))][(int)((long)(cc_1))] - matrix_1[(int)((long)(count_1))][(int)((long)(cc_1))] * fraction_1;
                        cc_1 = cc_1 + 1;
                    }
vector_1[(int)((long)(number_1))] = vector_1[(int)((long)(number_1))] - vector_1[(int)((long)(count_1))] * fraction_1;
                }
                number_1 = number_1 + 1;
            }
            count_1 = count_1 + 1;
        }
        String[] solution_1 = ((String[])(new String[]{}));
        count_1 = 0;
        while (count_1 < n_1) {
            double value_1 = vector_1[(int)((long)(count_1))] / matrix_1[(int)((long)(count_1))][(int)((long)(count_1))];
            solution_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(solution_1), java.util.stream.Stream.of(_p(value_1))).toArray(String[]::new)));
            count_1 = count_1 + 1;
        }
        String solved_1 = "f(x)=";
        count_1 = 0;
        while (count_1 < n_1) {
            String[] parts_1 = ((String[])(solution_1[(int)((long)(count_1))].split(java.util.regex.Pattern.quote("e"))));
            String coeff_1 = solution_1[(int)((long)(count_1))];
            if (parts_1.length > 1) {
                coeff_1 = parts_1[(int)((long)(0))] + "*10^" + parts_1[(int)((long)(1))];
            }
            solved_1 = solved_1 + "x^" + _p(n_1 - (count_1 + 1)) + "*" + coeff_1;
            if (count_1 + 1 != n_1) {
                solved_1 = solved_1 + "+";
            }
            count_1 = count_1 + 1;
        }
        return solved_1;
    }

    static void main() {
        System.out.println(points_to_polynomial(((long[][])(new long[][]{new long[]{1, 0}, new long[]{2, 0}, new long[]{3, 0}}))));
        System.out.println(points_to_polynomial(((long[][])(new long[][]{new long[]{1, 1}, new long[]{2, 1}, new long[]{3, 1}}))));
        System.out.println(points_to_polynomial(((long[][])(new long[][]{new long[]{1, 1}, new long[]{2, 4}, new long[]{3, 9}}))));
        System.out.println(points_to_polynomial(((long[][])(new long[][]{new long[]{1, 3}, new long[]{2, 6}, new long[]{3, 11}}))));
        System.out.println(points_to_polynomial(((long[][])(new long[][]{new long[]{1, -3}, new long[]{2, -6}, new long[]{3, -11}}))));
        System.out.println(points_to_polynomial(((long[][])(new long[][]{new long[]{1, 1}, new long[]{1, 2}, new long[]{1, 3}}))));
        System.out.println(points_to_polynomial(((long[][])(new long[][]{new long[]{1, 5}, new long[]{2, 2}, new long[]{3, 9}}))));
    }
    public static void main(String[] args) {
        main();
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
