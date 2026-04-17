import java.io.*;
import java.util.*;

public class Main {
    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int t = Integer.parseInt(lines.get(0).trim());
        int idx = 1;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int k = idx < lines.size() ? Integer.parseInt(lines.get(idx).trim()) : 0; idx++;
            List<Integer> vals = new ArrayList<>();
            for (int i = 0; i < k; i++) {
                int n = idx < lines.size() ? Integer.parseInt(lines.get(idx).trim()) : 0; idx++;
                for (int j = 0; j < n; j++) {
                    vals.add(idx < lines.size() ? Integer.parseInt(lines.get(idx).trim()) : 0); idx++;
                }
            }
            Collections.sort(vals);
            out.append('[');
            for (int i = 0; i < vals.size(); i++) {
                if (i > 0) out.append(',');
                out.append(vals.get(i));
            }
            out.append(']');
            if (tc + 1 < t) out.append('\n');
        }
        System.out.print(out.toString());
    }
}
