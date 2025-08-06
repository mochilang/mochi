public class Main {
    static String equation;

    static boolean is_digit(String ch) {
        return (ch.equals("0")) || (ch.equals("1")) || (ch.equals("2")) || (ch.equals("3")) || (ch.equals("4")) || (ch.equals("5")) || (ch.equals("6")) || (ch.equals("7")) || (ch.equals("8")) || (ch.equals("9"));
    }

    static int[] slice_without_last_int(int[] xs) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < xs.length - 1) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        return res;
    }

    static String[] slice_without_last_string(String[] xs) {
        String[] res_1 = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < xs.length - 1) {
            res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(xs[i_1])).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static int dijkstras_two_stack_algorithm(String equation) {
        int[] operand_stack = ((int[])(new int[]{}));
        String[] operator_stack = ((String[])(new String[]{}));
        int idx = 0;
        while (idx < _runeLen(equation)) {
            String ch = _substr(equation, idx, idx + 1);
            if (((Boolean)(is_digit(ch)))) {
                operand_stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(operand_stack), java.util.stream.IntStream.of(Integer.parseInt(ch))).toArray()));
            } else             if ((ch.equals("+")) || (ch.equals("-")) || (ch.equals("*")) || (ch.equals("/"))) {
                operator_stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(operator_stack), java.util.stream.Stream.of(ch)).toArray(String[]::new)));
            } else             if ((ch.equals(")"))) {
                String opr = operator_stack[operator_stack.length - 1];
                operator_stack = ((String[])(slice_without_last_string(((String[])(operator_stack)))));
                int num1 = operand_stack[operand_stack.length - 1];
                operand_stack = ((int[])(slice_without_last_int(((int[])(operand_stack)))));
                int num2 = operand_stack[operand_stack.length - 1];
                operand_stack = ((int[])(slice_without_last_int(((int[])(operand_stack)))));
                int total = (opr.equals("+")) ? num2 + num1 : (opr.equals("-")) ? num2 - num1 : (opr.equals("*")) ? num2 * num1 : num2 / num1;
                operand_stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(operand_stack), java.util.stream.IntStream.of(total)).toArray()));
            }
            idx = idx + 1;
        }
        return operand_stack[operand_stack.length - 1];
    }
    public static void main(String[] args) {
        equation = "(5 + ((4 * 2) * (2 + 3)))";
        System.out.println(equation + " = " + _p(dijkstras_two_stack_algorithm(equation)));
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
