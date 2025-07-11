import java.util.*;

class Alice {
    int alice;
    Alice(int alice) {
        this.alice = alice;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Alice other)) return false;
        return Objects.equals(this.alice, other.alice);
    }
    @Override public int hashCode() {
        return Objects.hash(alice);
    }
    int size() { return 1; }
}
public class MapAssign {
    public static void main(String[] args) {
    Alice scores = new Alice(1);
    ((Map)scores).put("bob", 2);
    System.out.println(scores.bob);
    }
}
