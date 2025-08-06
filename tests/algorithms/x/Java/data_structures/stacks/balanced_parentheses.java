public class Main {
    static String[] tests = new String[0];
    static int idx = 0;

    static String[] pop_last(String[] xs) {
        String[] res = ((String[])(new String[]{}));
        int i = 0;
        while (i < xs.length - 1) {
            res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[i])).toArray(String[]::new)));
            i = i + 1;
        }
        return res;
    }

    static boolean balanced_parentheses(String s) {
        String[] stack = ((String[])(new String[]{}));
        java.util.Map<String,String> pairs = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("(", ")"), java.util.Map.entry("[", "]"), java.util.Map.entry("{", "}")))));
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = s.substring(i_1, i_1+1);
            if (((Boolean)(pairs.containsKey(ch)))) {
                stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(ch)).toArray(String[]::new)));
            } else             if ((ch.equals(")")) || (ch.equals("]")) || (ch.equals("}"))) {
                if (stack.length == 0) {
                    return false;
                }
                String top = stack[stack.length - 1];
                if (!(((String)(pairs).get(top)).equals(ch))) {
                    return false;
                }
                stack = ((String[])(pop_last(((String[])(stack)))));
            }
            i_1 = i_1 + 1;
        }
        return stack.length == 0;
    }
    public static void main(String[] args) {
        tests = ((String[])(new String[]{"([]{})", "[()]{}{[()()]()}", "[(])", "1+2*3-4", ""}));
        idx = 0;
        while (idx < tests.length) {
            System.out.println(balanced_parentheses(tests[idx]));
            idx = idx + 1;
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
