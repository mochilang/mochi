public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String[] fields(String s) {
        String[] words = new String[]{};
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals(" ")) || (ch.equals("\t")) || (ch.equals("\n"))) {
                if (cur.length() > 0) {
                    words = java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                    cur = "";
                }
            } else {
                cur = cur + ch;
            }
            i = i + 1;
        }
        if (cur.length() > 0) {
            words = java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        }
        return words;
    }

    static String[] makePatterns() {
        String[] digits = new String[]{"1", "2", "3", "4", "5", "6", "7", "8", "9"};
        String[] pats = new String[]{};
        int i = 0;
        while (i < digits.length) {
            int j = 0;
            while (j < digits.length) {
                if (j != i) {
                    int k = 0;
                    while (k < digits.length) {
                        if (k != i && k != j) {
                            int l = 0;
                            while (l < digits.length) {
                                if (l != i && l != j && l != k) {
                                    pats = java.util.stream.Stream.concat(java.util.Arrays.stream(pats), java.util.stream.Stream.of(digits[i] + digits[j] + digits[k] + digits[l])).toArray(String[]::new);
                                }
                                l = l + 1;
                            }
                        }
                        k = k + 1;
                    }
                }
                j = j + 1;
            }
            i = i + 1;
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
            String guess = String.valueOf(patterns[0]);
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
                String pat = String.valueOf(patterns[idx]);
                int c = 0;
                int b = 0;
                int i = 0;
                while (i < 4) {
                    String cg = guess.substring(i, i + 1);
                    String cp = pat.substring(i, i + 1);
                    if ((cg.equals(cp))) {
                        b = b + 1;
                    } else                     if (((Number)(pat.indexOf(cg))).intValue() >= 0) {
                        c = c + 1;
                    }
                    i = i + 1;
                }
                if (c == cows && b == bulls) {
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
}
