import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Main {
    static int gcd(int a, int b) {
        while (b != 0) {
            int t = a % b;
            a = b;
            b = t;
        }
        return Math.abs(a);
    }

    static int maxPoints(int[][] points) {
        int n = points.length;
        if (n <= 2) return n;
        int best = 0;
        for (int i = 0; i < n; i++) {
            Map<String, Integer> slopes = new HashMap<>();
            int local = 0;
            for (int j = i + 1; j < n; j++) {
                int dx = points[j][0] - points[i][0];
                int dy = points[j][1] - points[i][1];
                int g = gcd(dx, dy);
                dx /= g;
                dy /= g;
                if (dx < 0) {
                    dx = -dx;
                    dy = -dy;
                } else if (dx == 0) {
                    dy = 1;
                } else if (dy == 0) {
                    dx = 1;
                }
                String key = dy + "/" + dx;
                int count = slopes.getOrDefault(key, 0) + 1;
                slopes.put(key, count);
                local = Math.max(local, count);
            }
            best = Math.max(best, local + 1);
        }
        return best;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int tc = Integer.parseInt(lines.get(0));
        int idx = 1;
        List<String> out = new ArrayList<>();
        for (int t = 0; t < tc; t++) {
            int n = Integer.parseInt(lines.get(idx++));
            int[][] points = new int[n][2];
            for (int i = 0; i < n; i++) {
                String[] parts = lines.get(idx++).split(" ");
                points[i][0] = Integer.parseInt(parts[0]);
                points[i][1] = Integer.parseInt(parts[1]);
            }
            out.add(Integer.toString(maxPoints(points)));
        }
        System.out.print(String.join("\n\n", out));
    }
}
