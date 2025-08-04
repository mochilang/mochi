public class Main {
    static String[] small;
    static String[] tens;
    static String[] illions;

    static String capitalize(String s) {
        if (_runeLen(s) == 0) {
            return s;
        }
        return _substr(s, 0, 1).toUpperCase() + _substr(s, 1, _runeLen(s));
    }

    static String say(int n) {
        String t = "";
        if (n < 0) {
            t = "negative ";
            n = -n;
        }
        if (n < 20) {
            return t + small[n];
        } else         if (n < 100) {
            t = tens[n / 10];
            int s = Math.floorMod(n, 10);
            if (s > 0) {
                t = t + "-" + small[s];
            }
            return t;
        } else         if (n < 1000) {
            t = small[n / 100] + " hundred";
            int s_1 = Math.floorMod(n, 100);
            if (s_1 > 0) {
                t = t + " " + String.valueOf(say(s_1));
            }
            return t;
        }
        String sx = "";
        int i = 0;
        int nn = n;
        while (nn > 0) {
            int p = Math.floorMod(nn, 1000);
            nn = nn / 1000;
            if (p > 0) {
                String ix = String.valueOf(say(p)) + illions[i];
                if (!(sx.equals(""))) {
                    ix = ix + " " + sx;
                }
                sx = ix;
            }
            i = i + 1;
        }
        return t + sx;
    }

    static String fourIsMagic(int n) {
        String s_2 = String.valueOf(say(n));
        s_2 = String.valueOf(capitalize(s_2));
        String t_1 = s_2;
        while (n != 4) {
            n = _runeLen(s_2);
            s_2 = String.valueOf(say(n));
            t_1 = t_1 + " is " + s_2 + ", " + s_2;
        }
        t_1 = t_1 + " is magic.";
        return t_1;
    }

    static void main() {
        int[] nums = ((int[])(new int[]{0, 4, 6, 11, 13, 75, 100, 337, -164, (int)9223372036854775807L}));
        for (int n : nums) {
            System.out.println(fourIsMagic(n));
        }
    }
    public static void main(String[] args) {
        small = ((String[])(new String[]{"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"}));
        tens = ((String[])(new String[]{"", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"}));
        illions = ((String[])(new String[]{"", " thousand", " million", " billion", " trillion", " quadrillion", " quintillion"}));
        main();
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
