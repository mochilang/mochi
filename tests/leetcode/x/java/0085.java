import java.io.*;

public class Main {
    static int hist(int[] h) {
        int best = 0;
        for (int i = 0; i < h.length; i++) {
            int mn = h[i];
            for (int j = i; j < h.length; j++) {
                if (h[j] < mn) mn = h[j];
                int area = mn * (j - i + 1);
                if (area > best) best = area;
            }
        }
        return best;
    }

    public static void main(String[] args) throws Exception {
        String data = new String(System.in.readAllBytes());
        String[] toks = data.trim().isEmpty() ? new String[0] : data.trim().split("\\s+");
        if (toks.length == 0) return;
        int idx = 0, t = Integer.parseInt(toks[idx++]);
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int rows = Integer.parseInt(toks[idx++]), cols = Integer.parseInt(toks[idx++]);
            int[] h = new int[cols];
            int best = 0;
            for (int r = 0; r < rows; r++) {
                String s = toks[idx++];
                for (int c = 0; c < cols; c++) h[c] = s.charAt(c) == '1' ? h[c] + 1 : 0;
                best = Math.max(best, hist(h));
            }
            if (tc > 0) out.append('\n');
            out.append(best);
        }
        System.out.print(out);
    }
}
