import java.util.*;

public class ExistsBuiltin {
    public static void main(String[] args) {
        List<Integer> data = Arrays.asList(1, 2);
        boolean flag = data.stream().anyMatch(x -> x == 1);
        System.out.println(flag);
    }
}
