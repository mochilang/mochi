import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    static int candy(int[] ratings) {
        int n = ratings.length;
        int[] candies = new int[n];
        for (int i = 0; i < n; i++) candies[i] = 1;
        for (int i = 1; i < n; i++) {
            if (ratings[i] > ratings[i - 1]) candies[i] = candies[i - 1] + 1;
        }
        for (int i = n - 2; i >= 0; i--) {
            if (ratings[i] > ratings[i + 1]) candies[i] = Math.max(candies[i], candies[i + 1] + 1);
        }
        int total = 0;
        for (int v : candies) total += v;
        return total;
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
            int[] ratings = new int[n];
            for (int i = 0; i < n; i++) ratings[i] = Integer.parseInt(lines.get(idx++));
            out.add(Integer.toString(candy(ratings)));
        }
        System.out.print(String.join("\n\n", out));
    }
}
