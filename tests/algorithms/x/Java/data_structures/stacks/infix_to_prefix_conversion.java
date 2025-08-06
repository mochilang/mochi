public class Main {
    static java.util.Map<String,Integer> PRIORITY;
    static String LETTERS;
    static String DIGITS;

    static boolean is_alpha(String ch) {
        int i = 0;
        while (i < _runeLen(LETTERS)) {
            if ((LETTERS.substring(i, i+1).equals(ch))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static boolean is_digit(String ch) {
        int i_1 = 0;
        while (i_1 < _runeLen(DIGITS)) {
            if ((DIGITS.substring(i_1, i_1+1).equals(ch))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static String reverse_string(String s) {
        String out = "";
        int i_2 = _runeLen(s) - 1;
        while (i_2 >= 0) {
            out = out + s.substring(i_2, i_2+1);
            i_2 = i_2 - 1;
        }
        return out;
    }

    static String infix_to_postfix(String infix) {
        String[] stack = ((String[])(new String[]{}));
        String[] post = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < _runeLen(infix)) {
            String x = infix.substring(i_3, i_3+1);
            if (((Boolean)(is_alpha(x))) || ((Boolean)(is_digit(x)))) {
                post = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(post), java.util.stream.Stream.of(x)).toArray(String[]::new)));
            } else             if ((x.equals("("))) {
                stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(x)).toArray(String[]::new)));
            } else             if ((x.equals(")"))) {
                if (stack.length == 0) {
                    throw new RuntimeException(String.valueOf("list index out of range"));
                }
                while (!(stack[stack.length - 1].equals("("))) {
                    post = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(post), java.util.stream.Stream.of(stack[stack.length - 1])).toArray(String[]::new)));
                    stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                }
                stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
            } else             if (stack.length == 0) {
                stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(x)).toArray(String[]::new)));
            } else {
                while (stack.length > 0 && !(stack[stack.length - 1].equals("(")) && (int)(((int)(PRIORITY).getOrDefault(x, 0))) <= (int)(((int)(PRIORITY).getOrDefault(stack[stack.length - 1], 0)))) {
                    post = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(post), java.util.stream.Stream.of(stack[stack.length - 1])).toArray(String[]::new)));
                    stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                }
                stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(x)).toArray(String[]::new)));
            }
            i_3 = i_3 + 1;
        }
        while (stack.length > 0) {
            if ((stack[stack.length - 1].equals("("))) {
                throw new RuntimeException(String.valueOf("invalid expression"));
            }
            post = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(post), java.util.stream.Stream.of(stack[stack.length - 1])).toArray(String[]::new)));
            stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
        }
        String res = "";
        int j = 0;
        while (j < post.length) {
            res = res + post[j];
            j = j + 1;
        }
        return res;
    }

    static String infix_to_prefix(String infix) {
        String reversed = "";
        int i_4 = _runeLen(infix) - 1;
        while (i_4 >= 0) {
            String ch = infix.substring(i_4, i_4+1);
            if ((ch.equals("("))) {
                reversed = reversed + ")";
            } else             if ((ch.equals(")"))) {
                reversed = reversed + "(";
            } else {
                reversed = reversed + ch;
            }
            i_4 = i_4 - 1;
        }
        String postfix = String.valueOf(infix_to_postfix(reversed));
        String prefix = String.valueOf(reverse_string(postfix));
        return prefix;
    }
    public static void main(String[] args) {
        PRIORITY = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("^", 3), java.util.Map.entry("*", 2), java.util.Map.entry("/", 2), java.util.Map.entry("%", 2), java.util.Map.entry("+", 1), java.util.Map.entry("-", 1)))));
        LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        DIGITS = "0123456789";
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
