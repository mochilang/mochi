import java.util.*;

public class ListAssign {
    public static void main(String[] args) {
    List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2));
    nums.set(1, 3);
    System.out.println(nums.get(1));
    }
}
