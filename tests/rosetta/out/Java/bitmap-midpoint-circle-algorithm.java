// bitmap-midpoint-circle-algorithm.mochi
import java.util.*;

public class BitmapMidpointCircleAlgorithm {
    static List<List<String>> g = circle(10);
    static List<List<String>> initGrid(int size) {
        List<List<String>> g = Arrays.asList();
        int y = 0;
        while (y < size) {
            List<String> row = Arrays.asList();
            int x = 0;
            while (x < size) {
                row.add(" ");
                x = (int)(x + 1);
            }
            g.add(row);
            y = (int)(y + 1);
        }
        return g;
    }
    static void set(List<List<String>> g, int x, int y) {
        if (x >= 0 && x < ((Number)g.get(0).size()).doubleValue() && y >= 0 && y < g.size()) {
            ((List)g.get(y)).set(x, "#");
        }
    }
    static List<List<String>> circle(int r) {
        int size = r * 2 + 1;
        List<List<String>> g = initGrid(size);
        int x = r;
        int y = 0;
        int err = 1 - r;
        while (y <= x) {
            set(g, r + x, r + y);
            set(g, r + y, r + x);
            set(g, r - x, r + y);
            set(g, r - y, r + x);
            set(g, r - x, r - y);
            set(g, r - y, r - x);
            set(g, r + x, r - y);
            set(g, r + y, r - x);
            y = (int)(y + 1);
            if (err < 0) {
                err = (int)(err + 2 * y + 1);
            }
            else {
                x = (int)(x - 1);
                err = (int)(err + 2 * (y - x) + 1);
            }
        }
        return g;
    }
    static String trimRight(List<String> row) {
        int end = row.size();
        while (end > 0 && Objects.equals(row.get(end - 1), " ")) {
            end = (int)(end - 1);
        }
        String s = "";
        int i = 0;
        while (i < end) {
            s = s + ((Number)row.get(i)).doubleValue();
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
    for (List<String> row : g) {
        System.out.println(trimRight(row));
    }
    }
}
