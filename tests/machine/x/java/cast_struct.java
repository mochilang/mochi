import java.util.*;

class Todo {
    String title;
    Todo(String title) {
        this.title = title;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Todo other)) return false;
        return Objects.equals(this.title, other.title);
    }
    @Override public int hashCode() {
        return Objects.hash(title);
    }
}
class Title {
    String title;
    Title(String title) {
        this.title = title;
    }
    int size() { return 1; }
}
public class CastStruct {
    public static void main(String[] args) {
    Todo todo = new Todo("hi");
    System.out.println(todo.title);
    }
}
