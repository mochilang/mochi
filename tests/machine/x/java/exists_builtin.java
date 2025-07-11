import java.util.*;

public class ExistsBuiltin {
    public static void main(String[] args) {
    List<Integer> data = new ArrayList<>(Arrays.asList(1, 2));
    Object flag = data.stream().anyMatch(x -> Objects.equals(x, 1));
    System.out.println(flag);
    }
}
