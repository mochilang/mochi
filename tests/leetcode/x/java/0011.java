import java.io.*;
import java.util.*;

public class Main {
    static int maxArea(int[] h) {
        int left = 0, right = h.length - 1, best = 0;
        while (left < right) {
            int height = Math.min(h[left], h[right]);
            best = Math.max(best, (right - left) * height);
            if (h[left] < h[right]) left++; else right--;
        }
        return best;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringBuilder sb = new StringBuilder();
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int t = Integer.parseInt(lines.get(0).trim());
        int idx = 1;
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(lines.get(idx++).trim());
            int[] h = new int[n];
            for (int i = 0; i < n; i++) h[i] = Integer.parseInt(lines.get(idx++).trim());
            if (tc > 0) sb.append('\n');
            sb.append(maxArea(h));
        }
        System.out.print(sb.toString());
    }
}
