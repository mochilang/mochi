import java.util.*;

public class TwoSum {
    static List<Integer> twoSum(List<Integer> nums, int target) {
        int n = nums.size();
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                if (Objects.equals(((Number)nums.get(i)).doubleValue() + ((Number)nums.get(j)).doubleValue(), target)) {
                    return Arrays.asList(i, j);
                }
            }
        }
        return Arrays.asList(-1, -1);
    }
    public static void main(String[] args) {
    List<Integer> result = twoSum(Arrays.asList(2, 7, 11, 15), 9);
    System.out.println(result.get(0));
    System.out.println(result.get(1));
    }
}
