// casting-out-nines.mochi
import java.util.*;

class BaseBeginEndKaprekar {
    int base;
    String begin;
    String end;
    List<String> kaprekar;
    BaseBeginEndKaprekar(int base, String begin, String end, List<String> kaprekar) {
        this.base = base;
        this.begin = begin;
        this.end = end;
        this.kaprekar = kaprekar;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof BaseBeginEndKaprekar other)) return false;
        return Objects.equals(this.base, other.base) && Objects.equals(this.begin, other.begin) && Objects.equals(this.end, other.end) && Objects.equals(this.kaprekar, other.kaprekar);
    }
    @Override public int hashCode() {
        return Objects.hash(base, begin, end, kaprekar);
    }
    int size() { return 4; }
}
public class CastingOutNines {
    static int parseIntBase(String s, int base) {
        String digits = "0123456789abcdefghijklmnopqrstuvwxyz";
        int n = 0;
        int i = 0;
        while (i < s.length()) {
            int j = 0;
            int v = 0;
            while (j < digits.length()) {
                if (Objects.equals(digits.substring(j, j + 1), s.substring(i, i + 1))) {
                    v = (int)(j);
                    break;
                }
                j = (int)(j + 1);
            }
            n = (int)(n * base + v);
            i = (int)(i + 1);
        }
        return n;
    }
    static String intToBase(int n, int base) {
        String digits = "0123456789abcdefghijklmnopqrstuvwxyz";
        if (n == 0) {
            return "0";
        }
        String out = "";
        int v = n;
        while (v > 0) {
            int d = v % base;
            out = digits.substring(d, d + 1) + out;
            v = (int)(v / base);
        }
        return out;
    }
    static List<String> subset(int base, String begin, String end) {
        int b = parseIntBase(begin, base);
        int e = parseIntBase(end, base);
        List<String> out = Arrays.asList();
        int k = b;
        while (k <= e) {
            String ks = intToBase(k, base);
            int mod = base - 1;
            int r1 = parseIntBase(ks, base) % mod;
            int r2 = (parseIntBase(ks, base) * parseIntBase(ks, base)) % mod;
            if (r1 == r2) {
                out.add(ks);
            }
            k = (int)(k + 1);
        }
        return out;
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    List<BaseBeginEndKaprekar> testCases = new ArrayList<>(Arrays.asList(new BaseBeginEndKaprekar(10, "1", "100", Arrays.asList("1", "9", "45", "55", "99")), new BaseBeginEndKaprekar(17, "10", "gg", Arrays.asList("3d", "d4", "gg"))));
    int idx = 0;
    while (idx < testCases.size()) {
        List<BaseBeginEndKaprekar> tc = testCases.get(idx);
        System.out.println(((Number)((Number)((Number)((Number)((Number)"\nTest case base = " + ((Number)String.valueOf(tc.get("base"))).doubleValue()).doubleValue() + ", begin = ").doubleValue() + ((Number)tc.get("begin")).doubleValue()).doubleValue() + ", end = ").doubleValue() + ((Number)tc.get("end")).doubleValue()).doubleValue() + ":");
        List<String> s = subset(tc.get("base"), tc.get("begin"), tc.get("end"));
        System.out.println("Subset:  " + String.valueOf(s));
        System.out.println("Kaprekar:" + ((Number)String.valueOf(tc.get("kaprekar"))).doubleValue());
        int sx = 0;
        boolean valid = true;
        int i = 0;
        while (i < ((Number)tc.get("kaprekar").size()).doubleValue()) {
            List<BaseBeginEndKaprekar> k = ((Map<?,?>)((List)tc.get("kaprekar"))).get(i);
            boolean found = false;
            while (sx < s.size()) {
                if (Objects.equals(s.get(sx), k)) {
                    found = true;
                    sx = (int)(sx + 1);
                    break;
                }
                sx = (int)(sx + 1);
            }
            if (!found) {
                System.out.println("Fail:" + k + " not in subset");
                valid = false;
                break;
            }
            i = (int)(i + 1);
        }
        if (valid) {
            System.out.println("Valid subset.");
        }
        idx = (int)(idx + 1);
    }
    }
}
