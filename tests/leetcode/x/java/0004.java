import java.io.*;
import java.util.*;

public class Main {
    static double median(int[] a, int[] b) {
        int[] m = new int[a.length + b.length];
        int i = 0, j = 0, k = 0;
        while (i < a.length && j < b.length) m[k++] = a[i] <= b[j] ? a[i++] : b[j++];
        while (i < a.length) m[k++] = a[i++];
        while (j < b.length) m[k++] = b[j++];
        if (k % 2 == 1) return m[k / 2];
        return (m[k / 2 - 1] + m[k / 2]) / 2.0;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line; while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int t = Integer.parseInt(lines.get(0).trim());
        int idx = 1;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(lines.get(idx++).trim());
            int[] a = new int[n]; for (int i=0;i<n;i++) a[i] = Integer.parseInt(lines.get(idx++).trim());
            int m = Integer.parseInt(lines.get(idx++).trim());
            int[] b = new int[m]; for (int i=0;i<m;i++) b[i] = Integer.parseInt(lines.get(idx++).trim());
            if (tc > 0) out.append('\n');
            out.append(String.format(java.util.Locale.US, "%.1f", median(a,b)));
        }
        System.out.print(out.toString());
    }
}
