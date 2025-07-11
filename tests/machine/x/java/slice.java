import java.util.*;

public class Slice {
    public static void main(String[] args) {
    System.out.println(((List)Arrays.asList(1, 2, 3)).subList(1, 3));
    System.out.println(((List)Arrays.asList(1, 2, 3)).subList(0, 2));
    System.out.println("hello".substring(1, 4));
    }
}
