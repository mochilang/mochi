import java.util.*;

class AB {
    int a;
    int b;
    AB(int a, int b) {
        this.a = a;
        this.b = b;
    }
    int size() { return 2; }
}
public class ForMapCollection {
    public static void main(String[] args) {
    AB m = new AB(1, 2);
    for (String k : Arrays.asList("a", "b")) {
        System.out.println(k);
    }
    }
}
