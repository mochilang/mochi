public class Main {

    static int parseIntDigits(String s) {
        int n = 0;
        int i = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i < _runeLen(s)) {
            String ch = s.substring(i, i + 1);
            if (!(Boolean)(digits.containsKey(ch))) {
                return 0;
            }
            n = n * 10 + (int)(((int)(digits).get(ch)));
            i = i + 1;
        }
        return n;
    }

    static int parseDC(String s) {
        boolean neg = false;
        if (_runeLen(s) > 0 && (s.substring(0, 1).equals("-"))) {
            neg = true;
            s = _substr(s, 1, _runeLen(s));
        }
        int dollars = 0;
        int cents = 0;
        int i = 0;
        boolean seenDot = false;
        int centDigits = 0;
        while (i < _runeLen(s)) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals("."))) {
                seenDot = true;
                i = i + 1;
                continue;
            }
            int d = parseIntDigits(ch);
            if (seenDot) {
                if (centDigits < 2) {
                    cents = cents * 10 + d;
                    centDigits = centDigits + 1;
                }
            } else {
                dollars = dollars * 10 + d;
            }
            i = i + 1;
        }
        if (centDigits == 1) {
            cents = cents * 10;
        }
        int val = dollars * 100 + cents;
        if (neg) {
            val = -val;
        }
        return val;
    }

    static int parseRate(String s) {
        boolean neg = false;
        if (_runeLen(s) > 0 && (s.substring(0, 1).equals("-"))) {
            neg = true;
            s = _substr(s, 1, _runeLen(s));
        }
        int whole = 0;
        int frac = 0;
        int digits = 0;
        boolean seenDot = false;
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals("."))) {
                seenDot = true;
                i = i + 1;
                continue;
            }
            int d = parseIntDigits(ch);
            if (seenDot) {
                if (digits < 4) {
                    frac = frac * 10 + d;
                    digits = digits + 1;
                }
            } else {
                whole = whole * 10 + d;
            }
            i = i + 1;
        }
        while (digits < 4) {
            frac = frac * 10;
            digits = digits + 1;
        }
        int val = whole * 10000 + frac;
        if (neg) {
            val = -val;
        }
        return val;
    }

    static String dcString(int dc) {
        int d = dc / 100;
        int n = dc;
        if (n < 0) {
            n = -n;
        }
        int c = Math.floorMod(n, 100);
        String cstr = String.valueOf(c);
        if (_runeLen(cstr) == 1) {
            cstr = "0" + cstr;
        }
        return String.valueOf(d) + "." + cstr;
    }

    static int extend(int dc, int n) {
        return dc * n;
    }

    static int tax(int total, int rate) {
        return ((Number)(((total * rate + 5000) / 10000))).intValue();
    }

    static String padLeft(String s, int n) {
        String out = s;
        while (_runeLen(out) < n) {
            out = " " + out;
        }
        return out;
    }

    static void main() {
        int hp = parseDC("5.50");
        int mp = parseDC("2.86");
        int rate = parseRate("0.0765");
        int totalBeforeTax = extend(hp, (int)4000000000000000L) + extend(mp, 2);
        int t = tax(totalBeforeTax, rate);
        int total = totalBeforeTax + t;
        System.out.println("Total before tax: " + String.valueOf(padLeft(String.valueOf(dcString(totalBeforeTax)), 22)));
        System.out.println("             Tax: " + String.valueOf(padLeft(String.valueOf(dcString(t)), 22)));
        System.out.println("           Total: " + String.valueOf(padLeft(String.valueOf(dcString(total)), 22)));
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
}
