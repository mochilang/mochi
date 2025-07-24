public class Main {

    static String[] fields(String s) {
        String[] words = new String[]{};
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if ((((ch.equals(" ")) || ch.equals("\n")) || ch.equals("\t"))) {
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

    static String join(String[] xs, String sep) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            if (i > 0) {
                res = res + sep;
            }
            res = res + xs[i];
            i = i + 1;
        }
        return res;
    }

    static String numberName(int n) {
        String[] small = new String[]{"no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"};
        String[] tens = new String[]{"ones", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"};
        if (n < 0) {
            return "";
        }
        if (n < 20) {
            return small[n];
        }
        if (n < 100) {
            String t = tens[((Number)((n / 10))).intValue()];
            int s = n % 10;
            if (s > 0) {
                t = t + " " + small[s];
            }
            return t;
        }
        return "";
    }

    static String pluralizeFirst(String s, int n) {
        if (n == 1) {
            return s;
        }
        String[] w = fields(s);
        if (w.length > 0) {
w[0] = w[0] + "s";
        }
        return join(w, " ");
    }

    static int randInt(int seed, int n) {
        int next = (seed * 1664525 + 1013904223) % 2147483647;
        return next % n;
    }

    static String slur(String p, int d) {
        if (p.length() <= 2) {
            return p;
        }
        String[] a = new String[]{};
        int i = 1;
        while (i < p.length() - 1) {
            a = java.util.stream.Stream.concat(java.util.Arrays.stream(a), java.util.stream.Stream.of(p.substring(i, i + 1))).toArray(String[]::new);
            i = i + 1;
        }
        int idx = a.length - 1;
        int seed = d;
        while (idx >= 1) {
            seed = (seed * 1664525 + 1013904223) % 2147483647;
            if (seed % 100 >= d) {
                int j = seed % (idx + 1);
                String tmp = a[idx];
a[idx] = a[j];
a[j] = tmp;
            }
            idx = idx - 1;
        }
        String s = p.substring(0, 1);
        int k = 0;
        while (k < a.length) {
            s = s + a[k];
            k = k + 1;
        }
        s = s + p.substring(p.length() - 1, p.length());
        String[] w = fields(s);
        return join(w, " ");
    }

    static void main() {
        int i = 99;
        while (i > 0) {
            System.out.println(slur(numberName(i), i) + " " + pluralizeFirst(slur("bottle of", i), i) + " " + slur("beer on the wall", i));
            System.out.println(slur(numberName(i), i) + " " + pluralizeFirst(slur("bottle of", i), i) + " " + slur("beer", i));
            System.out.println(slur("take one", i) + " " + slur("down", i) + " " + slur("pass it around", i));
            System.out.println(slur(numberName(i - 1), i) + " " + pluralizeFirst(slur("bottle of", i), i - 1) + " " + slur("beer on the wall", i));
            i = i - 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
