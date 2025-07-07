import java.util.*;
public class ListNestedAssign {
    public static void main(String[] args) {
        List<List<Integer>> matrix = new ArrayList<>();
        matrix.add(new ArrayList<>(Arrays.asList(1,2)));
        matrix.add(new ArrayList<>(Arrays.asList(3,4)));
        matrix.get(1).set(0,5);
        System.out.println(matrix.get(1).get(0));
    }
}
