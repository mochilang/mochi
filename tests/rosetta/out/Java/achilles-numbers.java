// achilles-numbers.mochi
import java.util.*;

public class AchillesNumbers {
    static Map<Integer,Boolean> pps = new LinkedHashMap<Integer,Boolean>();
    static int pow10(int exp) {
        int n = 1;
        int i = 0;
        while (i < exp) {
            n = (int)(n * 10);
            i = (int)(i + 1);
        }
        return n;
    }
    static int totient(int n) {
        int tot = n;
        int nn = n;
        int i = 2;
        while (i * i <= nn) {
            if (Objects.equals(nn % i, 0)) {
                while (Objects.equals(nn % i, 0)) {
                    nn = (int)(nn / i);
                }
                tot = (int)(tot - tot / i);
            }
            if (i == 2) {
                i = (int)(1);
            }
            i = (int)(i + 2);
        }
        if (nn > 1) {
            tot = (int)(tot - tot / nn);
        }
        return tot;
    }
    static void getPerfectPowers(int maxExp) {
        int upper = pow10(maxExp);
        int i = 2;
        while (i * i < upper) {
            int p = i * i;
            while (true) {
                p = (int)(p * i);
                if (p >= upper) {
                    break;
                }
                pps.put(p, true);
            }
            i = (int)(i + 1);
        }
    }
    static Map<Integer,Boolean> getAchilles(int minExp, int maxExp) {
        int lower = pow10(minExp);
        int upper = pow10(maxExp);
        Map<Integer,Boolean> achilles = new LinkedHashMap<Integer,Boolean>();
        int b = 1;
        while (b * b * b < upper) {
            int b3 = b * b * b;
            int a = 1;
            while (true) {
                int p = b3 * a * a;
                if (p >= upper) {
                    break;
                }
                if (p >= lower) {
                    if (!(pps.containsKey(p))) {
                        achilles.put(p, true);
                    }
                }
                a = (int)(a + 1);
            }
            b = (int)(b + 1);
        }
        return achilles;
    }
    static List<Integer> sortInts(List<Integer> xs) {
        List<Integer> res = Arrays.asList();
        List<Integer> tmp = xs;
        while (tmp.size() > 0) {
            List<Integer> min = tmp.get(0);
            int idx = 0;
            int i = 1;
            while (i < tmp.size()) {
                if (String.valueOf(tmp.get(i)).compareTo(String.valueOf(min)) < 0) {
                    min = tmp.get(i);
                    idx = (int)(i);
                }
                i = (int)(i + 1);
            }
            res = res + Arrays.asList(min);
            List<Integer> out = Arrays.asList();
            int j = 0;
            while (j < tmp.size()) {
                if (j != idx) {
                    out = out + ((Number)Arrays.asList(tmp.get(j))).doubleValue();
                }
                j = (int)(j + 1);
            }
            tmp = out;
        }
        return res;
    }
    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }
    static void main() {
        int maxDigits = 15;
        getPerfectPowers(maxDigits);
        Map<Integer,Boolean> achSet = getAchilles(1, 5);
        List<Integer> ach = Arrays.asList();
        for (Number k : (List<Number>)((Map<Integer,Boolean>)achSet).get("keys")()) {
            ach = ach + Arrays.asList(k);
        }
        ach = sortInts(ach);
        System.out.println("First 50 Achilles numbers:");
        int i = 0;
        while (i < 50) {
            String line = "";
            int j = 0;
            while (j < 10) {
                line = line + ((Number)pad(ach.get(i), 4)).doubleValue();
                if (j < 9) {
                    line = line + " ";
                }
                i = (int)(i + 1);
                j = (int)(j + 1);
            }
            System.out.println(line);
        }
        System.out.println("\nFirst 30 strong Achilles numbers:");
        List<Integer> strong = Arrays.asList();
        int count = 0;
        int idx = 0;
        while (count < 30) {
            int tot = totient(ach.get(idx));
            if (achSet.containsKey(tot)) {
                strong = strong + ((Number)Arrays.asList(ach.get(idx))).doubleValue();
                count = (int)(count + 1);
            }
            idx = (int)(idx + 1);
        }
        i = (int)(0);
        while (i < 30) {
            String line = "";
            int j = 0;
            while (j < 10) {
                line = line + ((Number)pad(strong.get(i), 5)).doubleValue();
                if (j < 9) {
                    line = line + " ";
                }
                i = (int)(i + 1);
                j = (int)(j + 1);
            }
            System.out.println(line);
        }
        System.out.println("\nNumber of Achilles numbers with:");
        List<Integer> counts = new ArrayList<>(Arrays.asList(1, 12, 47, 192, 664, 2242, 7395, 24008, 77330, 247449, 788855, 2508051, 7960336, 25235383));
        int d = 2;
        while (d <= maxDigits) {
            List<Integer> c = counts.get(d - 2);
            System.out.println(pad(d, 2) + " digits: " + String.valueOf(c));
            d = (int)(d + 1);
        }
    }
    static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
    static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
        LinkedHashMap<K,V> m = new LinkedHashMap<>();
        for (var e : entries) m.put(e.getKey(), e.getValue());
        return m;
    }
    public static void main(String[] args) {
    main();
    }
}
