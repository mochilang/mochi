public class Main {

    static boolean contains_int(int[] xs, int x) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == x) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String[] split(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        String current = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            if ((ch.equals(sep))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch;
            }
            i_1 = i_1 + 1;
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return res;
    }

    static double pow_int_float(int base, int exp) {
        double result = 1.0;
        int i_2 = 0;
        while (i_2 < exp) {
            result = result * (((Number)(base)).doubleValue());
            i_2 = i_2 + 1;
        }
        return result;
    }

    static String points_to_polynomial(int[][] coordinates) {
        if (coordinates.length == 0) {
            throw new RuntimeException(String.valueOf("The program cannot work out a fitting polynomial."));
        }
        int i_3 = 0;
        while (i_3 < coordinates.length) {
            if (coordinates[i_3].length != 2) {
                throw new RuntimeException(String.valueOf("The program cannot work out a fitting polynomial."));
            }
            i_3 = i_3 + 1;
        }
        int j = 0;
        while (j < coordinates.length) {
            int k = j + 1;
            while (k < coordinates.length) {
                if (coordinates[j][0] == coordinates[k][0] && coordinates[j][1] == coordinates[k][1]) {
                    throw new RuntimeException(String.valueOf("The program cannot work out a fitting polynomial."));
                }
                k = k + 1;
            }
            j = j + 1;
        }
        int[] set_x = ((int[])(new int[]{}));
        i_3 = 0;
        while (i_3 < coordinates.length) {
            int x_val = coordinates[i_3][0];
            if (!(Boolean)contains_int(((int[])(set_x)), x_val)) {
                set_x = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(set_x), java.util.stream.IntStream.of(x_val)).toArray()));
            }
            i_3 = i_3 + 1;
        }
        if (set_x.length == 1) {
            return "x=" + _p(_geti(coordinates[0], 0));
        }
        if (set_x.length != coordinates.length) {
            throw new RuntimeException(String.valueOf("The program cannot work out a fitting polynomial."));
        }
        int n = coordinates.length;
        double[][] matrix = ((double[][])(new double[][]{}));
        int row = 0;
        while (row < n) {
            double[] line = ((double[])(new double[]{}));
            int col = 0;
            while (col < n) {
                double power = pow_int_float(coordinates[row][0], n - (col + 1));
                line = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(line), java.util.stream.DoubleStream.of(power)).toArray()));
                col = col + 1;
            }
            matrix = ((double[][])(appendObj(matrix, line)));
            row = row + 1;
        }
        double[] vector = ((double[])(new double[]{}));
        row = 0;
        while (row < n) {
            vector = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(vector), java.util.stream.DoubleStream.of(((double)(coordinates[row][1])))).toArray()));
            row = row + 1;
        }
        int count = 0;
        while (count < n) {
            int number = 0;
            while (number < n) {
                if (count != number) {
                    double fraction = matrix[number][count] / matrix[count][count];
                    int cc = 0;
                    while (cc < n) {
matrix[number][cc] = matrix[number][cc] - matrix[count][cc] * fraction;
                        cc = cc + 1;
                    }
vector[number] = vector[number] - vector[count] * fraction;
                }
                number = number + 1;
            }
            count = count + 1;
        }
        String[] solution = ((String[])(new String[]{}));
        count = 0;
        while (count < n) {
            double value = vector[count] / matrix[count][count];
            solution = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(solution), java.util.stream.Stream.of(_p(value))).toArray(String[]::new)));
            count = count + 1;
        }
        String solved = "f(x)=";
        count = 0;
        while (count < n) {
            String[] parts = ((String[])(solution[count].split(java.util.regex.Pattern.quote("e"))));
            String coeff = solution[count];
            if (parts.length > 1) {
                coeff = parts[0] + "*10^" + parts[1];
            }
            solved = solved + "x^" + _p(n - (count + 1)) + "*" + coeff;
            if (count + 1 != n) {
                solved = solved + "+";
            }
            count = count + 1;
        }
        return solved;
    }

    static void main() {
        System.out.println(points_to_polynomial(((int[][])(new int[][]{new int[]{1, 0}, new int[]{2, 0}, new int[]{3, 0}}))));
        System.out.println(points_to_polynomial(((int[][])(new int[][]{new int[]{1, 1}, new int[]{2, 1}, new int[]{3, 1}}))));
        System.out.println(points_to_polynomial(((int[][])(new int[][]{new int[]{1, 1}, new int[]{2, 4}, new int[]{3, 9}}))));
        System.out.println(points_to_polynomial(((int[][])(new int[][]{new int[]{1, 3}, new int[]{2, 6}, new int[]{3, 11}}))));
        System.out.println(points_to_polynomial(((int[][])(new int[][]{new int[]{1, -3}, new int[]{2, -6}, new int[]{3, -11}}))));
        System.out.println(points_to_polynomial(((int[][])(new int[][]{new int[]{1, 1}, new int[]{1, 2}, new int[]{1, 3}}))));
        System.out.println(points_to_polynomial(((int[][])(new int[][]{new int[]{1, 5}, new int[]{2, 2}, new int[]{3, 9}}))));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
