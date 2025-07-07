class Todo {
    String title;
    Todo(String title) {
        this.title = title;
    }
}

public class CastStruct {
    public static void main(String[] args) {
        Todo todo = new Todo("hi");
        System.out.println(todo.title);
    }
}
