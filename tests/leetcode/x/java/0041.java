import java.io.*;
import java.util.*;

public class Main {
    static int firstMissingPositive(int[] nums) {
        int n = nums.length;
        int i = 0;
        while (i < n) {
            int v = nums[i];
            if (v >= 1 && v <= n && nums[v - 1] != v) {
                int tmp = nums[i];
                nums[i] = nums[v - 1];
                nums[v - 1] = tmp;
            } else {
                i++;
            }
        }
        for (i = 0; i < n; i++) if (nums[i] != i + 1) return i + 1;
        return n + 1;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line.trim());
        if (lines.isEmpty() || lines.get(0).isEmpty()) return;
        int idx = 0;
        int t = Integer.parseInt(lines.get(idx++));
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(lines.get(idx++));
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = Integer.parseInt(lines.get(idx++));
            if (tc > 0) out.append('\n');
            out.append(firstMissingPositive(nums));
        }
        System.out.print(out);
    }
}
