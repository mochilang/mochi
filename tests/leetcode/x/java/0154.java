import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    static int findMin(int[] nums) {
        int left = 0;
        int right = nums.length - 1;
        while (left < right) {
            int mid = (left + right) / 2;
            if (nums[mid] < nums[right]) right = mid;
            else if (nums[mid] > nums[right]) left = mid + 1;
            else right--;
        }
        return nums[left];
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
            int[] nums = new int[n];
            for (int i = 0; i < n; i++) nums[i] = Integer.parseInt(lines.get(idx++));
            out.add(Integer.toString(findMin(nums)));
        }
        System.out.print(String.join("\n\n", out));
    }
}
