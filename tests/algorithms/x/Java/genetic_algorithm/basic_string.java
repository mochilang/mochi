public class Main {
    static class PairString {
        String first;
        String second;
        PairString(String first, String second) {
            this.first = first;
            this.second = second;
        }
        PairString() {}
        @Override public String toString() {
            return String.format("{'first': '%s', 'second': '%s'}", String.valueOf(first), String.valueOf(second));
        }
    }


    static int evaluate(String item, String target) {
        int score = 0;
        int i = 0;
        while (i < _runeLen(item) && i < _runeLen(target)) {
            if ((_substr(item, i, i + 1).equals(_substr(target, i, i + 1)))) {
                score = score + 1;
            }
            i = i + 1;
        }
        return score;
    }

    static PairString crossover(String parent1, String parent2) {
        int cut = _runeLen(parent1) / 2;
        String child1 = _substr(parent1, 0, cut) + _substr(parent2, cut, _runeLen(parent2));
        String child2 = _substr(parent2, 0, cut) + _substr(parent1, cut, _runeLen(parent1));
        return new PairString(child1, child2);
    }

    static String mutate(String child, String[] genes) {
        if (_runeLen(child) == 0) {
            return child;
        }
        String gene = genes[0];
        return _substr(child, 0, _runeLen(child) - 1) + gene;
    }

    static void main() {
        System.out.println(_p(evaluate("Helxo Worlx", "Hello World")));
        PairString pair = crossover("123456", "abcdef");
        System.out.println(pair.first);
        System.out.println(pair.second);
        String mut = String.valueOf(mutate("123456", ((String[])(new String[]{"A", "B", "C", "D", "E", "F"}))));
        System.out.println(mut);
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
