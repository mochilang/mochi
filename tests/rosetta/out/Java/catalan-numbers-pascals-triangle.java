// catalan-numbers-pascals-triangle.mochi
import java.util.*;

public class CatalanNumbersPascalsTriangle {
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    int n = 15;
    List<Integer> t = Arrays.asList();
    for (int unused0 = 0; unused0 < (n + 2); unused0++) {
        t.add(0);
    }
    t.set(1, 1);
    for (int i = 1; i < (n + 1); i++) {
        Object j = i;
        while (j > 1) {
            t.set(j, ((Number)t.get(j)).doubleValue() + ((Number)t.get(j - 1)).doubleValue());
            j = j - 1;
        }
        t.set(i + 1, t.get(i));
        j = i + 1;
        while (j > 1) {
            t.set(j, ((Number)t.get(j)).doubleValue() + ((Number)t.get(j - 1)).doubleValue());
            j = j - 1;
        }
        int cat = ((Number)t.get(i + 1)).doubleValue() - ((Number)t.get(i)).doubleValue();
        if (i < 10) {
            System.out.println(" " + String.valueOf(i) + " : " + String.valueOf(cat));
        }
        else {
            System.out.println(String.valueOf(i) + " : " + String.valueOf(cat));
        }
    }
    }
}
