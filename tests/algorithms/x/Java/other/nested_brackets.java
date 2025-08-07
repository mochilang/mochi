public class Main {
    static java.util.Map<String,String> OPEN_TO_CLOSED;

    static String[] slice_without_last(String[] xs) {
        String[] res = ((String[])(new String[]{}));
        int i = 0;
        while (i < xs.length - 1) {
            res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[i])).toArray(String[]::new)));
            i = i + 1;
        }
        return res;
    }

    static boolean is_balanced(String s) {
        String[] stack = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String symbol = _substr(s, i_1, i_1 + 1);
            if (((Boolean)(OPEN_TO_CLOSED.containsKey(symbol)))) {
                stack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(symbol)).toArray(String[]::new)));
            } else             if ((symbol.equals(")")) || (symbol.equals("]")) || (symbol.equals("}"))) {
                if (stack.length == 0) {
                    return false;
                }
                String top = stack[stack.length - 1];
                if (!(((String)(OPEN_TO_CLOSED).get(top)).equals(symbol))) {
                    return false;
                }
                stack = ((String[])(slice_without_last(((String[])(stack)))));
            }
            i_1 = i_1 + 1;
        }
        return stack.length == 0;
    }

    static void main() {
        System.out.println(is_balanced(""));
        System.out.println(is_balanced("()"));
        System.out.println(is_balanced("[]"));
        System.out.println(is_balanced("{}"));
        System.out.println(is_balanced("()[]{}"));
        System.out.println(is_balanced("(())"));
        System.out.println(is_balanced("[["));
        System.out.println(is_balanced("([{}])"));
        System.out.println(is_balanced("(()[)]"));
        System.out.println(is_balanced("([)]"));
        System.out.println(is_balanced("[[()]]"));
        System.out.println(is_balanced("(()(()))"));
        System.out.println(is_balanced("]"));
        System.out.println(is_balanced("Life is a bowl of cherries."));
        System.out.println(is_balanced("Life is a bowl of che{}ies."));
        System.out.println(is_balanced("Life is a bowl of che}{ies."));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            OPEN_TO_CLOSED = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("(", ")"), java.util.Map.entry("[", "]"), java.util.Map.entry("{", "}")))));
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
