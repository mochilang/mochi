// 99-bottles-of-beer-2.mochi
import java.util.*;

public class M99BottlesOfBeer2 {
    static List<String> fields(String s) {
        List<String> words = Arrays.asList();
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if (Objects.equals(ch, " ") || Objects.equals(ch, "\n") || Objects.equals(ch, "\t")) {
                if (cur.length() > 0) {
                    words.add(cur);
                    cur = "";
                }
            }
            else {
                cur = cur + ch;
            }
            i = (int)(i + 1);
        }
        if (cur.length() > 0) {
            words.add(cur);
        }
        return words;
    }
    static String join(List<String> xs, String sep) {
        String res = "";
        int i = 0;
        while (i < xs.size()) {
            if (i > 0) {
                res = res + sep;
            }
            res = res + ((Number)xs.get(i)).doubleValue();
            i = (int)(i + 1);
        }
        return res;
    }
    static String numberName(int n) {
        List<String> small = new ArrayList<>(Arrays.asList("no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"));
        List<String> tens = new ArrayList<>(Arrays.asList("ones", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"));
        if (n < 0) {
            return "";
        }
        if (n < 20) {
            return small.get(n);
        }
        if (n < 100) {
            List<String> t = tens.get(Integer.parseInt((n / 10)));
            int s = n % 10;
            if (s > 0) {
                t = t + " " + ((Number)small.get(s)).doubleValue();
            }
            return t;
        }
        return "";
    }
    static String pluralizeFirst(String s, int n) {
        if (n == 1) {
            return s;
        }
        List<String> w = fields(s);
        if (w.size() > 0) {
            w.set(0, ((Number)w.get(0)).doubleValue() + "s");
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
        List<String> a = Arrays.asList();
        int i = 1;
        while (i < p.length() - 1) {
            a.add(p.substring(i, i + 1));
            i = (int)(i + 1);
        }
        int idx = a.size() - 1;
        int seed = d;
        while (idx >= 1) {
            seed = (int)((seed * 1664525 + 1013904223) % 2147483647);
            if (seed % 100 >= d) {
                int j = seed % (idx + 1);
                List<String> tmp = a.get(idx);
                a.set(idx, a.get(j));
                a.set(j, tmp);
            }
            idx = (int)(idx - 1);
        }
        String s = p.substring(0, 1);
        int k = 0;
        while (k < a.size()) {
            s = s + ((Number)a.get(k)).doubleValue();
            k = (int)(k + 1);
        }
        s = s + p.substring(p.length() - 1, p.length());
        List<String> w = fields(s);
        return join(w, " ");
    }
    static void main() {
        int i = 99;
        while (i > 0) {
            System.out.println(slur(numberName(i), i) + " " + pluralizeFirst(slur("bottle of", i), i) + " " + slur("beer on the wall", i));
            System.out.println(slur(numberName(i), i) + " " + pluralizeFirst(slur("bottle of", i), i) + " " + slur("beer", i));
            System.out.println(slur("take one", i) + " " + slur("down", i) + " " + slur("pass it around", i));
            System.out.println(slur(numberName(i - 1), i) + " " + pluralizeFirst(slur("bottle of", i), i - 1) + " " + slur("beer on the wall", i));
            i = (int)(i - 1);
        }
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    main();
    }
}
