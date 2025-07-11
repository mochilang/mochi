import java.util.*;

class Inner {
    int inner;
    Inner(int inner) {
        this.inner = inner;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Inner other)) return false;
        return Objects.equals(this.inner, other.inner);
    }
    @Override public int hashCode() {
        return Objects.hash(inner);
    }
    int size() { return 1; }
}
class Outer {
    Inner outer;
    Outer(Inner outer) {
        this.outer = outer;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Outer other)) return false;
        return Objects.equals(this.outer, other.outer);
    }
    @Override public int hashCode() {
        return Objects.hash(outer);
    }
    int size() { return 1; }
}
public class MapNestedAssign {
    public static void main(String[] args) {
    Outer data = new Outer(new Inner(1));
    data.outer.inner = 2;
    System.out.println(data.outer.inner);
    }
}
