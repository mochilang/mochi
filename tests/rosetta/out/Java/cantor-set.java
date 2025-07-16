// cantor-set.mochi
import java.util.*;

class StartLenIndex {
    int start;
    int len;
    int index;
    StartLenIndex(int start, int len, int index) {
        this.start = start;
        this.len = len;
        this.index = index;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof StartLenIndex other)) return false;
        return Objects.equals(this.start, other.start) && Objects.equals(this.len, other.len) && Objects.equals(this.index, other.index);
    }
    @Override public int hashCode() {
        return Objects.hash(start, len, index);
    }
    int size() { return 3; }
}
public class CantorSet {
    static String setChar(String s, int idx, String ch) {
        return s.substring(0, idx) + ch + s.substring(idx + 1, s.length());
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    int width = 81;
    int height = 5;
    List<String> lines = Arrays.asList();
    for (int i = 0; i < height; i++) {
        String row = "";
        int j = 0;
        while (j < width) {
            row = row + "*";
            j = (int)(j + 1);
        }
        lines.add(row);
    }
    List<Map<String,Integer>> stack = Arrays.asList(new StartLenIndex(0, width, 1));
    while (stack.size() > 0) {
        List<Map<String,Integer>> frame = stack.get(stack.size() - 1);
        stack = ((List)stack).subList(0, stack.size() - 1);
        List<Map<String,Integer>> start = frame.get("start");
        List<Map<String,Integer>> lenSeg = frame.get("len");
        List<Map<String,Integer>> index = frame.get("index");
        int seg = Integer.parseInt((lenSeg / 3));
        if (seg == 0) {
            continue;
        }
        List<Map<String,Integer>> i = index;
        while (i < height) {
            int j = start + seg;
            while (j < start + 2 * seg) {
                lines.set(i, setChar(lines.get(i), j, " "));
                j = (int)(j + 1);
            }
            i = i + 1;
        }
        stack.add(new StartLenIndex(start, seg, index + 1));
        stack.add(new StartLenIndex(start + seg * 2, seg, index + 1));
    }
    for (String line : lines) {
        System.out.println(line);
    }
    }
}
