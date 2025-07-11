import java.util.*;

class AB {
    int a;
    int b;
    AB(int a, int b) {
        this.a = a;
        this.b = b;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof AB other)) return false;
        return Objects.equals(this.a, other.a) && Objects.equals(this.b, other.b);
    }
    @Override public int hashCode() {
        return Objects.hash(a, b);
    }
    int size() { return 2; }
}
public class MapIndex {
    public static void main(String[] args) {
    AB m = new AB(1, 2);
    System.out.println(m.b);
    }
}
