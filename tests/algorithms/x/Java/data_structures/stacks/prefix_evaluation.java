public class Main {
    static String test_expression;
    static String test_expression2;
    static String test_expression3;

    static String[] split(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        String current = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if ((ch.equals(sep))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch;
            }
            i = i + 1;
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return res;
    }

    static String[] tokenize(String s) {
        String[] parts = ((String[])(s.split(java.util.regex.Pattern.quote(" "))));
        String[] res_1 = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < parts.length) {
            String p = parts[i_1];
            if (!(p.equals(""))) {
                res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(p)).toArray(String[]::new)));
            }
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static boolean is_digit(String ch) {
        return (ch.compareTo("0") >= 0) && (ch.compareTo("9") <= 0);
    }

    static boolean is_operand(String token) {
        if ((token.equals(""))) {
            return false;
        }
        int i_2 = 0;
        while (i_2 < _runeLen(token)) {
            String ch_1 = _substr(token, i_2, i_2 + 1);
            if (!(Boolean)is_digit(ch_1)) {
                return false;
            }
            i_2 = i_2 + 1;
        }
        return true;
    }

    static int to_int(String token) {
        int res_2 = 0;
        int i_3 = 0;
        while (i_3 < _runeLen(token)) {
            res_2 = res_2 * 10 + (Integer.parseInt(_substr(token, i_3, i_3 + 1)));
            i_3 = i_3 + 1;
        }
        return res_2;
    }

    static double apply_op(String op, double a, double b) {
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
        return 0.0;
    }

    static double evaluate(String expression) {
        String[] tokens = ((String[])(tokenize(expression)));
        double[] stack = ((double[])(new double[]{}));
        int i_4 = tokens.length - 1;
        while (i_4 >= 0) {
            String token = tokens[i_4];
            if (!(token.equals(""))) {
                if (((Boolean)(is_operand(token)))) {
                    stack = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stack), java.util.stream.DoubleStream.of((((Number)(to_int(token))).doubleValue()))).toArray()));
                } else {
                    double o1 = stack[stack.length - 1];
                    double o2 = stack[stack.length - 2];
                    stack = ((double[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 2)));
                    double res_3 = apply_op(token, o1, o2);
                    stack = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stack), java.util.stream.DoubleStream.of(res_3)).toArray()));
                }
            }
            i_4 = i_4 - 1;
        }
        return stack[0];
    }

    static double[] eval_rec(String[] tokens, int pos) {
        String token_1 = tokens[pos];
        int next = pos + 1;
        if (((Boolean)(is_operand(token_1)))) {
            return new double[]{(((Number)(to_int(token_1))).doubleValue()), (((Number)(next)).doubleValue())};
        }
        double[] left = ((double[])(eval_rec(((String[])(tokens)), next)));
        double a = left[0];
        int p1 = ((int)(left[1]));
        double[] right = ((double[])(eval_rec(((String[])(tokens)), p1)));
        double b = right[0];
        double p2 = right[1];
        return new double[]{apply_op(token_1, a, b), p2};
    }

    static double evaluate_recursive(String expression) {
        String[] tokens_1 = ((String[])(tokenize(expression)));
        double[] res_4 = ((double[])(eval_rec(((String[])(tokens_1)), 0)));
        return res_4[0];
    }
    public static void main(String[] args) {
        test_expression = "+ 9 * 2 6";
        System.out.println(_p(evaluate(test_expression)));
        test_expression2 = "/ * 10 2 + 4 1 ";
        System.out.println(_p(evaluate(test_expression2)));
        test_expression3 = "+ * 2 3 / 8 4";
        System.out.println(_p(evaluate_recursive(test_expression3)));
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
