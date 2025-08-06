public class Main {

    static double[] slice_without_last(double[] xs) {
        double[] res = ((double[])(new double[]{}));
        int i = 0;
        while (i < xs.length - 1) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        return res;
    }

    static double parse_float(String token) {
        double sign = 1.0;
        int idx = 0;
        if (_runeLen(token) > 0) {
            String first = _substr(token, 0, 1);
            if ((first.equals("-"))) {
                sign = -1.0;
                idx = 1;
            } else             if ((first.equals("+"))) {
                idx = 1;
            }
        }
        int int_part = 0;
        while (idx < _runeLen(token) && !(_substr(token, idx, idx + 1).equals("."))) {
            int_part = int_part * 10 + Integer.parseInt(_substr(token, idx, idx + 1));
            idx = idx + 1;
        }
        double result = 1.0 * int_part;
        if (idx < _runeLen(token) && (_substr(token, idx, idx + 1).equals("."))) {
            idx = idx + 1;
            double place = 0.1;
            while (idx < _runeLen(token)) {
                int digit = Integer.parseInt(_substr(token, idx, idx + 1));
                result = result + place * (1.0 * digit);
                place = place / 10.0;
                idx = idx + 1;
            }
        }
        return sign * result;
    }

    static double pow_float(double base, double exp) {
        double result_1 = 1.0;
        int i_1 = 0;
        int e = ((Number)(exp)).intValue();
        while (i_1 < e) {
            result_1 = result_1 * base;
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static double apply_op(double a, double b, String op) {
        if ((op.equals("+"))) {
            return a + b;
        }
        if ((op.equals("-"))) {
            return a - b;
        }
        if ((op.equals("*"))) {
            return a * b;
        }
        if ((op.equals("/"))) {
            return a / b;
        }
        if ((op.equals("^"))) {
            return pow_float(a, b);
        }
        return 0.0;
    }

    static double evaluate(String[] tokens) {
        if (tokens.length == 0) {
            return 0.0;
        }
        double[] stack = ((double[])(new double[]{}));
        for (String token : tokens) {
            if ((token.equals("+")) || (token.equals("-")) || (token.equals("*")) || (token.equals("/")) || (token.equals("^"))) {
                if (((token.equals("+")) || (token.equals("-"))) && stack.length < 2) {
                    double b = stack[stack.length - 1];
                    stack = ((double[])(slice_without_last(((double[])(stack)))));
                    if ((token.equals("-"))) {
                        stack = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stack), java.util.stream.DoubleStream.of(0.0 - b)).toArray()));
                    } else {
                        stack = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stack), java.util.stream.DoubleStream.of(b)).toArray()));
                    }
                } else {
                    double b_1 = stack[stack.length - 1];
                    stack = ((double[])(slice_without_last(((double[])(stack)))));
                    double a = stack[stack.length - 1];
                    stack = ((double[])(slice_without_last(((double[])(stack)))));
                    double result_2 = apply_op(a, b_1, token);
                    stack = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stack), java.util.stream.DoubleStream.of(result_2)).toArray()));
                }
            } else {
                stack = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stack), java.util.stream.DoubleStream.of(parse_float(token))).toArray()));
            }
        }
        if (stack.length != 1) {
            throw new RuntimeException(String.valueOf("Invalid postfix expression"));
        }
        return stack[0];
    }
    public static void main(String[] args) {
        System.out.println(_p(evaluate(((String[])(new String[]{"2", "1", "+", "3", "*"})))));
        System.out.println(_p(evaluate(((String[])(new String[]{"4", "13", "5", "/", "+"})))));
        System.out.println(_p(evaluate(((String[])(new String[]{"5", "6", "9", "*", "+"})))));
        System.out.println(_p(evaluate(((String[])(new String[]{"2", "-", "3", "+"})))));
        System.out.println(_p(evaluate(((String[])(new String[]{})))));
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
}
