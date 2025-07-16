// chaos-game.mochi
import java.util.*;

public class ChaosGame {
    static List<Integer> randInt(int s, int n) {
        int next = (s * 1664525 + 1013904223) % 2147483647;
        return Arrays.asList(next, next % n);
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    int width = 60;
    int height = Integer.parseInt((Double.parseDouble(String.valueOf(width)) * 0.866025));
    int iterations = 5000;
    List<List<String>> grid = Arrays.asList();
    int y = 0;
    while (y < height) {
        List<String> line = Arrays.asList();
        int x = 0;
        while (x < width) {
            line.add(" ");
            x = (int)(x + 1);
        }
        grid.add(line);
        y = (int)(y + 1);
    }
    int seed = 1;
    List<List<Integer>> vertices = Arrays.asList(Arrays.asList(0, height - 1), Arrays.asList(width - 1, height - 1), Arrays.asList(Integer.parseInt((width / 2)), 0));
    int px = Integer.parseInt((width / 2));
    int py = Integer.parseInt((height / 2));
    int i = 0;
    while (i < iterations) {
        List<Integer> r = randInt(seed, 3);
        seed = (int)(r.get(0));
        int idx = Integer.parseInt(((List)r.get(1)));
        List<List<Integer>> v = vertices.get(idx);
        px = (int)(Integer.parseInt((((Number)(px + ((Number)v.get(0)).doubleValue())).doubleValue() / 2)));
        py = (int)(Integer.parseInt((((Number)(py + ((Number)v.get(1)).doubleValue())).doubleValue() / 2)));
        if (px >= 0 && px < width && py >= 0 && py < height) {
            ((List)grid.get(py)).set(px, "*");
        }
        i = (int)(i + 1);
    }
    y = (int)(0);
    while (y < height) {
        String line = "";
        int x = 0;
        while (x < width) {
            line = line + ((Number)((List)grid.get(y)).get(x)).doubleValue();
            x = (int)(x + 1);
        }
        System.out.println(line);
        y = (int)(y + 1);
    }
    }
}
