public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String[] fields(String s) {
        String[] words = new String[]{};
        String cur = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            if ((ch.equals(" ")) || (ch.equals("\t")) || (ch.equals("\n"))) {
                if (_runeLen(cur) > 0) {
                    words = java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                    cur = "";
                }
            } else {
                cur = cur + ch;
            }
            i_1 = i_1 + 1;
        }
        if (_runeLen(cur) > 0) {
            words = java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        }
        return words;
    }

    static String[] makePatterns() {
        String[] digits = new String[]{"1", "2", "3", "4", "5", "6", "7", "8", "9"};
        String[] pats = new String[]{};
        int i_2 = 0;
        while (i_2 < digits.length) {
            int j = 0;
            while (j < digits.length) {
                if (j != i_2) {
                    int k = 0;
                    while (k < digits.length) {
                        if (k != i_2 && k != j) {
                            int l = 0;
                            while (l < digits.length) {
                                if (l != i_2 && l != j && l != k) {
                                    pats = java.util.stream.Stream.concat(java.util.Arrays.stream(pats), java.util.stream.Stream.of(digits[i_2] + digits[j] + digits[k] + digits[l])).toArray(String[]::new);
                                }
                                l = l + 1;
                            }
                        }
                        k = k + 1;
                    }
                }
                j = j + 1;
            }
            i_2 = i_2 + 1;
        }
        return pats;
    }

    static void main() {
        System.out.println("Cows and bulls/player\n" + "You think of four digit number of unique digits in the range 1 to 9.\n" + "I guess.  You score my guess:\n" + "    A correct digit but not in the correct place is a cow.\n" + "    A correct digit in the correct place is a bull.\n" + "You give my score as two numbers separated with a space.");
        String[] patterns = makePatterns();
        while (true) {
            if (patterns.length == 0) {
                System.out.println("Oops, check scoring.");
                return;
            }
            String guess = patterns[0];
            patterns = java.util.Arrays.copyOfRange(patterns, 1, patterns.length);
            int cows = 0;
            int bulls = 0;
            while (true) {
                System.out.println("My guess: " + guess + ".  Score? (c b) ");
                String line = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
                String[] toks = fields(line);
                if (toks.length == 2) {
                    int c = Integer.parseInt(toks[0]);
                    int b = Integer.parseInt(toks[1]);
                    if (c >= 0 && c <= 4 && b >= 0 && b <= 4 && c + b <= 4) {
                        cows = c;
                        bulls = b;
                        break;
                    }
                }
                System.out.println("Score guess as two numbers: cows bulls");
            }
            if (bulls == 4) {
                System.out.println("I did it. :)");
                return;
            }
            String[] next = new String[]{};
            int idx = 0;
            while (idx < patterns.length) {
                String pat = patterns[idx];
                int c_1 = 0;
                int b_1 = 0;
                int i_3 = 0;
                while (i_3 < 4) {
                    String cg = _substr(guess, i_3, i_3 + 1);
                    String cp = _substr(pat, i_3, i_3 + 1);
                    if ((cg.equals(cp))) {
                        b_1 = b_1 + 1;
                    } else                     if (((Number)(pat.indexOf(cg))).intValue() >= 0) {
                        c_1 = c_1 + 1;
                    }
                    i_3 = i_3 + 1;
                }
                if (c_1 == cows && b_1 == bulls) {
                    next = java.util.stream.Stream.concat(java.util.Arrays.stream(next), java.util.stream.Stream.of(pat)).toArray(String[]::new);
                }
                idx = idx + 1;
            }
            patterns = next;
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
