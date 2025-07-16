// binary-strings.mochi
import java.util.*;

public class BinaryStrings {
    static int i = 0;
    static String char(int n) {
        String letters = "abcdefghijklmnopqrstuvwxyz";
        int idx = n - 97;
        if (idx < 0 || idx >= letters.length()) {
            return "?";
        }
        return letters.substring(idx, idx + 1);
    }
    static String fromBytes(List<Integer> bs) {
        String s = "";
        int i = 0;
        while (i < bs.size()) {
            s = s + ((Number)char(bs.get(i))).doubleValue();
            i = (int)(i + 1);
        }
        return s;
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    List<Integer> b = Arrays.asList(98, 105, 110, 97, 114, 121);
    System.out.println(String.valueOf(b));
    List<Integer> c = b;
    System.out.println(String.valueOf(c));
    System.out.println(String.valueOf(Objects.equals(b, c)));
    List<Integer> d = Arrays.asList();
    while (i < b.size()) {
        d.add(b.get(i));
        i = (int)(i + 1);
    }
    d.set(1, 97);
    d.set(4, 110);
    System.out.println(fromBytes(b));
    System.out.println(fromBytes(d));
    System.out.println(String.valueOf(Objects.equals(b.size(), 0)));
    Object z = append(b, 122);
    System.out.println(fromBytes(z));
    List<Integer> sub = ((List)b).subList(1, 3);
    System.out.println(fromBytes(sub));
    List<Integer> f = Arrays.asList();
    i = (int)(0);
    while (i < d.size()) {
        List<Integer> val = d.get(i);
        if (Objects.equals(val, 110)) {
            f.add(109);
        }
        else {
            f.add(val);
        }
        i = (int)(i + 1);
    }
    System.out.println(fromBytes(d) + " -> " + fromBytes(f));
    List<Integer> rem = Arrays.asList();
    rem.add(b.get(0));
    i = (int)(3);
    while (i < b.size()) {
        rem.add(b.get(i));
        i = (int)(i + 1);
    }
    System.out.println(fromBytes(rem));
    }
}
