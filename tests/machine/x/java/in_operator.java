// Generated by Mochi compiler v0.10.28 on 1970-01-01T00:00:00Z
// in_operator.mochi
import java.util.*;

public class InOperator {
    public static void main(String[] args) {
        List<Integer> xs = new ArrayList<>(Arrays.asList(1, 2, 3));
        System.out.println(xs.contains(2));
        System.out.println(!((Boolean)(xs.contains(5))));
    }
}
