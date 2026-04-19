import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    static int minCut(String s) {
        int n = s.length();
        boolean[][] pal = new boolean[n][n];
        int[] cuts = new int[n];
        for (int end = 0; end < n; end++) {
            cuts[end] = end;
            for (int start = 0; start <= end; start++) {
                if (s.charAt(start) == s.charAt(end) && (end - start <= 2 || pal[start + 1][end - 1])) {
                    pal[start][end] = true;
                    if (start == 0) cuts[end] = 0;
                    else cuts[end] = Math.min(cuts[end], cuts[start - 1] + 1);
                }
            }
        }
        return cuts[n - 1];
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int tc = Integer.parseInt(lines.get(0));
        List<String> out = new ArrayList<>();
        for (int i = 1; i <= tc; i++) out.add(Integer.toString(minCut(lines.get(i))));
        System.out.print(String.join("\n\n", out));
    }
}
