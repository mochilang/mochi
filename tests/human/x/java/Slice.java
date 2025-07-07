import java.util.*;
public class Slice {
    public static void main(String[] args) {
        List<Integer> list = Arrays.asList(1, 2, 3);
        List<Integer> sub1 = list.subList(1, 3);
        System.out.println(sub1.get(0) + " " + sub1.get(1));
        List<Integer> sub2 = list.subList(0, 2);
        System.out.println(sub2.get(0) + " " + sub2.get(1));
        String s = "hello";
        System.out.println(s.substring(1, 4));
    }
}
