// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// call-a-function-3.mochi
import java.util.*;

public class CallAFunction3 {
    static List<Object> f() {
        return Arrays.asList(0, 0.000000);
    }
    static int g(int a, double b) {
        return 0;
    }
    static void h(String s, List<Integer> nums) {
    }
    static void main() {
        h("ex1", Arrays.asList());
        h("ex2", Arrays.asList(1, 2));
        h("ex3", Arrays.asList(1, 2, 3, 4));
        List<Integer> list = new ArrayList<>(Arrays.asList(1, 2, 3, 4));
        h("ex4", list);
    }
    public static void main(String[] args) {
        main();
    }
}
