import java.util.*;

public class InOperator {
    public static void main(String[] args) {
        List<Integer> xs = Arrays.asList(1, 2, 3);
        System.out.println(xs.contains(2));
        System.out.println(!xs.contains(5));
    }
}
