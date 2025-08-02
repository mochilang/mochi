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

    static String[] shuffle(String[] xs) {
        String[] arr = xs;
        int i_1 = arr.length - 1;
        while (i_1 > 0) {
            int j = Math.floorMod(_now(), (i_1 + 1));
            String tmp = arr[i_1];
arr[i_1] = arr[j];
arr[j] = tmp;
            i_1 = i_1 - 1;
        }
        return arr;
    }

    static void main() {
        System.out.println("Cows and Bulls");
        System.out.println("Guess four digit number of unique digits in the range 1 to 9.");
        System.out.println("A correct digit but not in the correct place is a cow.");
        System.out.println("A correct digit in the correct place is a bull.");
        String[] digits = new String[]{"1", "2", "3", "4", "5", "6", "7", "8", "9"};
        digits = shuffle(digits);
        String pat = digits[0] + digits[1] + digits[2] + digits[3];
        String valid = "123456789";
        while (true) {
            System.out.println("Guess: ");
            String guess = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            if (_runeLen(guess) != 4) {
                System.out.println("Please guess a four digit number.");
                continue;
            }
            int cows = 0;
            int bulls = 0;
            String seen = "";
            int i_2 = 0;
            boolean malformed = false;
            while (i_2 < 4) {
                String cg = _substr(guess, i_2, i_2 + 1);
                if (((Number)(seen.indexOf(cg))).intValue() != (-1)) {
                    System.out.println("Repeated digit: " + cg);
                    malformed = true;
                    break;
                }
                seen = seen + cg;
                int pos = ((Number)(pat.indexOf(cg))).intValue();
                if (pos == (-1)) {
                    if (((Number)(valid.indexOf(cg))).intValue() == (-1)) {
                        System.out.println("Invalid digit: " + cg);
                        malformed = true;
                        break;
                    }
                } else                 if (pos == i_2) {
                    bulls = bulls + 1;
                } else {
                    cows = cows + 1;
                }
                i_2 = i_2 + 1;
            }
            if (malformed) {
                continue;
            }
            System.out.println("Cows: " + _p(cows) + ", bulls: " + _p(bulls));
            if (bulls == 4) {
                System.out.println("You got it.");
                break;
            }
        }
    }
    public static void main(String[] args) {
        main();
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
