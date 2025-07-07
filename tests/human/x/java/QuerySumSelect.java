import java.util.*;

public class QuerySumSelect {
    public static void main(String[] args) {
        List<Integer> nums = Arrays.asList(1,2,3);
        int sum = 0;
        for(int n : nums) if(n > 1) sum += n;
        System.out.println(sum);
    }
}
