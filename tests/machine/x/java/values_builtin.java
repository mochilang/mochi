import java.util.*;

class ABC {
    int a;
    int b;
    int c;
    ABC(int a, int b, int c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ABC other)) return false;
        return Objects.equals(this.a, other.a) && Objects.equals(this.b, other.b) && Objects.equals(this.c, other.c);
    }
    @Override public int hashCode() {
        return Objects.hash(a, b, c);
    }
    int size() { return 3; }
}
public class ValuesBuiltin {
    public static void main(String[] args) {
    ABC m = new ABC(1, 2, 3);
    System.out.println(Arrays.asList(m.a, m.b, m.c));
    }
}
