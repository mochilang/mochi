public class Main {
    static java.util.Map<String,Integer> PRECEDENCES;
    static java.util.Map<String,String> ASSOCIATIVITIES;

    static int precedence(String ch) {
        if (PRECEDENCES.containsKey(ch)) {
            return ((int)(PRECEDENCES).getOrDefault(ch, 0));
        }
        return -1;
    }

    static String associativity(String ch) {
        if (ASSOCIATIVITIES.containsKey(ch)) {
            return ((String)(ASSOCIATIVITIES).get(ch));
        }
        return "";
    }

    static boolean balanced_parentheses(String expr) {
        int count = 0;
        int i = 0;
        while (i < _runeLen(expr)) {
            String ch = _substr(expr, i, i + 1);
            if ((ch.equals("("))) {
                count = count + 1;
            }
            if ((ch.equals(")"))) {
                count = count - 1;
                if (count < 0) {
                    return false;
                }
            }
            i = i + 1;
        }
        return count == 0;
    }

    static boolean is_letter(String ch) {
        return (("a".compareTo(ch) <= 0) && (ch.compareTo("z") <= 0)) || (("A".compareTo(ch) <= 0) && (ch.compareTo("Z") <= 0));
    }

    static boolean is_digit(String ch) {
        return ("0".compareTo(ch) <= 0) && (ch.compareTo("9") <= 0);
    }

    static boolean is_alnum(String ch) {
        return ((Boolean)(is_letter(ch))) || ((Boolean)(is_digit(ch)));
    }

    static String infix_to_postfix(String expression) {
        if (((Boolean)(balanced_parentheses(expression))) == false) {
            throw new RuntimeException(String.valueOf("Mismatched parentheses"));
        }
        String[] stack = ((String[])(new String[]{}));
        String[] postfix = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < _runeLen(expression)) {
            String ch_1 = _substr(expression, i_1, i_1 + 1);
            if (((Boolean)(is_alnum(ch_1)))) {
                postfix = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(postfix), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
            } else             if ((ch_1.equals("("))) {
                stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
            } else             if ((ch_1.equals(")"))) {
                while (stack.length > 0 && !(stack[stack.length - 1].equals("("))) {
                    postfix = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(postfix), java.util.stream.Stream.of(stack[stack.length - 1])).toArray(String[]::new)));
                    stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                }
                if (stack.length > 0) {
                    stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                }
            } else             if ((ch_1.equals(" "))) {
            } else {
                while (true) {
                    if (stack.length == 0) {
                        stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
                        break;
                    }
                    int cp = precedence(ch_1);
                    int tp = precedence(stack[stack.length - 1]);
                    if (cp > tp) {
                        stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
                        break;
                    }
                    if (cp < tp) {
                        postfix = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(postfix), java.util.stream.Stream.of(stack[stack.length - 1])).toArray(String[]::new)));
                        stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                        continue;
                    }
                    if ((associativity(ch_1).equals("RL"))) {
                        stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
                        break;
                    }
                    postfix = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(postfix), java.util.stream.Stream.of(stack[stack.length - 1])).toArray(String[]::new)));
                    stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                }
            }
            i_1 = i_1 + 1;
        }
        while (stack.length > 0) {
            postfix = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(postfix), java.util.stream.Stream.of(stack[stack.length - 1])).toArray(String[]::new)));
            stack = ((String[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
        }
        String res = "";
        int j = 0;
        while (j < postfix.length) {
            if (j > 0) {
                res = res + " ";
            }
            res = res + postfix[j];
            j = j + 1;
        }
        return res;
    }

    static void main() {
        String expression = "a+b*(c^d-e)^(f+g*h)-i";
        System.out.println(expression);
        System.out.println(infix_to_postfix(expression));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PRECEDENCES = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("+", 1), java.util.Map.entry("-", 1), java.util.Map.entry("*", 2), java.util.Map.entry("/", 2), java.util.Map.entry("^", 3)))));
            ASSOCIATIVITIES = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("+", "LR"), java.util.Map.entry("-", "LR"), java.util.Map.entry("*", "LR"), java.util.Map.entry("/", "LR"), java.util.Map.entry("^", "RL")))));
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
