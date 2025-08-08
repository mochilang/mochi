public class Main {
    static class Case {
        String text;
        boolean expected;
        Case(String text, boolean expected) {
            this.text = text;
            this.expected = expected;
        }
        Case() {}
        @Override public String toString() {
            return String.format("{'text': '%s', 'expected': %s}", String.valueOf(text), String.valueOf(expected));
        }
    }

    static Case[] test_data;

    static String reverse(String s) {
        String res = "";
        int i = _runeLen(s) - 1;
        while (i >= 0) {
            res = res + s.substring(i, i+1);
            i = i - 1;
        }
        return res;
    }

    static boolean is_palindrome(String s) {
        int start_i = 0;
        int end_i = _runeLen(s) - 1;
        while (start_i < end_i) {
            if ((s.substring(start_i, start_i+1).equals(s.substring(end_i, end_i+1)))) {
                start_i = start_i + 1;
                end_i = end_i - 1;
            } else {
                return false;
            }
        }
        return true;
    }

    static boolean is_palindrome_traversal(String s) {
        int end = Math.floorDiv(_runeLen(s), 2);
        int n = _runeLen(s);
        int i_1 = 0;
        while (i_1 < end) {
            if (!(s.substring(i_1, i_1+1).equals(s.substring(n - i_1 - 1, n - i_1 - 1+1)))) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static boolean is_palindrome_recursive(String s) {
        if (_runeLen(s) <= 1) {
            return true;
        }
        if ((s.substring(0, 0+1).equals(s.substring(_runeLen(s) - 1, _runeLen(s) - 1+1)))) {
            return is_palindrome_recursive(s.substring(1, _runeLen(s) - 1));
        }
        return false;
    }

    static boolean is_palindrome_slice(String s) {
        return (s.equals(reverse(s)));
    }

    static void main() {
        for (Case t : test_data) {
            String s = t.text;
            boolean expected = t.expected;
            boolean r1 = is_palindrome(s);
            boolean r2 = is_palindrome_traversal(s);
            boolean r3 = is_palindrome_recursive(s);
            boolean r4 = is_palindrome_slice(s);
            if (r1 != expected || r2 != expected || r3 != expected || r4 != expected) {
                throw new RuntimeException(String.valueOf("algorithm mismatch"));
            }
            System.out.println(s + " " + _p(expected));
        }
        System.out.println("a man a plan a canal panama");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            test_data = ((Case[])(new Case[]{new Case("MALAYALAM", true), new Case("String", false), new Case("rotor", true), new Case("level", true), new Case("A", true), new Case("BB", true), new Case("ABC", false), new Case("amanaplanacanalpanama", true)}));
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
