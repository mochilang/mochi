import java.util.*;

public class AvgBuiltin {
    public static void main(String[] args) {
        List<Integer> nums = Arrays.asList(1, 2, 3);
        double avg = nums.stream().mapToInt(Integer::intValue).average().orElse(0);
        System.out.println(avg);
    }
}
