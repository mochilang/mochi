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
            if (pairs.containsKey(ch)) {
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
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            tests = ((String[])(new String[]{"([]{})", "[()]{}{[()()]()}", "[(])", "1+2*3-4", ""}));
            idx = 0;
            while (idx < tests.length) {
                System.out.println(balanced_parentheses(tests[idx]));
                idx = idx + 1;
            }
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
}
