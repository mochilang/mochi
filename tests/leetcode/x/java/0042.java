import java.io.*;
import java.util.*;

public class Main {
    static int trap(int[] height) {
        int left = 0, right = height.length - 1, leftMax = 0, rightMax = 0, water = 0;
        while (left <= right) {
            if (leftMax <= rightMax) {
                if (height[left] < leftMax) water += leftMax - height[left];
                else leftMax = height[left];
                left++;
            } else {
                if (height[right] < rightMax) water += rightMax - height[right];
                else rightMax = height[right];
                right--;
            }
        }
        return water;
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
            int[] arr = new int[n];
            for (int i = 0; i < n; i++) arr[i] = Integer.parseInt(lines.get(idx++));
            if (tc > 0) out.append('\n');
            out.append(trap(arr));
        }
        System.out.print(out);
    }
}
