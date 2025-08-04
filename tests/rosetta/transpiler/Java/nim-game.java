public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i < _runeLen(str)) {
            n = n * 10 + (int)(((int)(digits).getOrDefault(str.substring(i, i + 1), 0)));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static void showTokens(int tokens) {
        System.out.println("Tokens remaining " + _p(tokens));
    }

    static void main() {
        int tokens = 12;
        boolean done = false;
        while (!done) {
            showTokens(tokens);
            System.out.println("");
            System.out.println("How many tokens 1, 2 or 3?");
            String line = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            int t = 0;
            if (_runeLen(line) > 0) {
                t = Integer.parseInt(line);
            }
            if (t < 1 || t > 3) {
                System.out.println("\nMust be a number between 1 and 3, try again.\n");
            } else {
                int ct = 4 - t;
                String s = "s";
                if (ct == 1) {
                    s = "";
                }
                System.out.println("  Computer takes " + _p(ct) + " token" + s + "\n\n");
                tokens = tokens - 4;
            }
            if (tokens == 0) {
                showTokens(0);
                System.out.println("  Computer wins!");
                done = true;
            }
        }
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
