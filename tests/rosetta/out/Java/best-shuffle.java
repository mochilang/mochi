// best-shuffle.mochi
import java.util.*;

public class BestShuffle {
    static int nextRand(int seed) {
        return (seed * 1664525 + 1013904223) % 2147483647;
    }
    static List<any> shuffleChars(String s, int seed) {
        List<String> chars = Arrays.asList();
        int i = 0;
        while (i < s.length()) {
            chars.add(s.substring(i, i + 1));
            i = (int)(i + 1);
        }
        int sd = seed;
        int idx = chars.size() - 1;
        while (idx > 0) {
            sd = (int)(nextRand(sd));
            int j = sd % (idx + 1);
            List<String> tmp = chars.get(idx);
            chars.set(idx, chars.get(j));
            chars.set(j, tmp);
            idx = (int)(idx - 1);
        }
        String res = "";
        i = (int)(0);
        while (i < chars.size()) {
            res = res + ((Number)chars.get(i)).doubleValue();
            i = (int)(i + 1);
        }
        return Arrays.asList(res, sd);
    }
    static List<any> bestShuffle(String s, int seed) {
        List<any> r = shuffleChars(s, seed);
        List<any> t = r.get(0);
        List<any> sd = r.get(1);
        List<String> arr = Arrays.asList();
        int i = 0;
        while (i < t.size()) {
            arr.add(t.substring(i, i + 1));
            i = (int)(i + 1);
        }
        i = (int)(0);
        while (i < arr.size()) {
            int j = 0;
            while (j < arr.size()) {
                if (i != j && !Objects.equals(arr.get(i), s.substring(j, j + 1)) && !Objects.equals(arr.get(j), s.substring(i, i + 1))) {
                    List<String> tmp = arr.get(i);
                    arr.set(i, arr.get(j));
                    arr.set(j, tmp);
                    break;
                }
                j = (int)(j + 1);
            }
            i = (int)(i + 1);
        }
        int count = 0;
        i = (int)(0);
        while (i < arr.size()) {
            if (Objects.equals(arr.get(i), s.substring(i, i + 1))) {
                count = (int)(count + 1);
            }
            i = (int)(i + 1);
        }
        String out = "";
        i = (int)(0);
        while (i < arr.size()) {
            out = out + ((Number)arr.get(i)).doubleValue();
            i = (int)(i + 1);
        }
        return Arrays.asList(out, sd, count);
    }
    static void main() {
        List<String> ts = new ArrayList<>(Arrays.asList("abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"));
        int seed = 1;
        int i = 0;
        while (i < ts.size()) {
            List<any> r = bestShuffle(ts.get(i), seed);
            List<any> shuf = r.get(0);
            seed = (int)(r.get(1));
            List<any> cnt = r.get(2);
            System.out.println(((Number)((Number)((Number)((Number)((Number)ts.get(i)).doubleValue() + " -> ").doubleValue() + shuf).doubleValue() + " (").doubleValue() + String.valueOf(cnt)).doubleValue() + ")");
            i = (int)(i + 1);
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
