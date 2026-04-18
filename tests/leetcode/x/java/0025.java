import java.io.*;
import java.util.*;

public class Main {
    static List<Integer> revGroups(List<Integer> arr, int k) {
        List<Integer> out = new ArrayList<>(arr);
        for (int i = 0; i + k <= out.size(); i += k) {
            for (int l = i, r = i + k - 1; l < r; l++, r--) {
                int tmp = out.get(l);
                out.set(l, out.get(r));
                out.set(r, tmp);
            }
        }
        return out;
    }
    static String fmtList(List<Integer> arr) {
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < arr.size(); i++) { if (i > 0) sb.append(','); sb.append(arr.get(i)); }
        sb.append(']');
        return sb.toString();
    }
    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line; while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int idx = 0, t = Integer.parseInt(lines.get(idx++).trim());
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = idx < lines.size() ? Integer.parseInt(lines.get(idx++).trim()) : 0;
            List<Integer> arr = new ArrayList<>();
            for (int i = 0; i < n; i++) arr.add(idx < lines.size() ? Integer.parseInt(lines.get(idx++).trim()) : 0);
            int k = idx < lines.size() ? Integer.parseInt(lines.get(idx++).trim()) : 1;
            out.append(fmtList(revGroups(arr, k)));
            if (tc + 1 < t) out.append('\n');
        }
        System.out.print(out.toString());
    }
}
