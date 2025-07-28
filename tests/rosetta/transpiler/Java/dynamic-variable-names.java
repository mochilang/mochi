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
            n = n * 10 + (int)(((int)(digits).get(str.substring(i, i + 1))));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static void main() {
        int n = 0;
        while (n < 1 || n > 5) {
            System.out.println("How many integer variables do you want to create (max 5) : ");
            String line = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            if (_runeLen(line) > 0) {
                n = Integer.parseInt(line);
            }
        }
        java.util.Map<String,Integer> vars = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        System.out.println("OK, enter the variable names and their values, below\n");
        int i = 1;
        while (i <= n) {
            System.out.println("\n  Variable " + String.valueOf(i) + "\n");
            System.out.println("    Name  : ");
            String name = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            if (vars.containsKey(name)) {
                System.out.println("  Sorry, you've already created a variable of that name, try again");
                continue;
            }
            int value = 0;
            while (true) {
                System.out.println("    Value : ");
                String valstr = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
                if (_runeLen(valstr) == 0) {
                    System.out.println("  Not a valid integer, try again");
                    continue;
                }
                boolean ok = true;
                int j = 0;
                boolean neg = false;
                if ((valstr.substring(0, 1).equals("-"))) {
                    neg = true;
                    j = 1;
                }
                while (j < _runeLen(valstr)) {
                    String ch = valstr.substring(j, j + 1);
                    if ((ch.compareTo("0") < 0) || (ch.compareTo("9") > 0)) {
                        ok = false;
                        break;
                    }
                    j = j + 1;
                }
                if (!ok) {
                    System.out.println("  Not a valid integer, try again");
                    continue;
                }
                value = Integer.parseInt(valstr);
                break;
            }
vars.put(name, value);
            i = i + 1;
        }
        System.out.println("\nEnter q to quit");
        while (true) {
            System.out.println("\nWhich variable do you want to inspect : ");
            String name = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            if ((name.toLowerCase().equals("q"))) {
                return;
            }
            if (vars.containsKey(name)) {
                System.out.println("It's value is " + String.valueOf(((int)(vars).getOrDefault(name, 0))));
            } else {
                System.out.println("Sorry there's no variable of that name, try again");
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
}
