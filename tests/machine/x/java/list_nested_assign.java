import java.util.*;

public class ListNestedAssign {
    public static void main(String[] args) {
    List<List<Integer>> matrix = new ArrayList<>(Arrays.asList(Arrays.asList(1, 2), Arrays.asList(3, 4)));
    ((List)matrix.get(1)).set(0, 5);
    System.out.println(((List)matrix.get(1)).get(0));
    }
}
