public class Main {
    static class Todo {
        String title;
        Todo(String title) {
            this.title = title;
        }
    }

    static Todo todo = new Todo("hi");
    static class Data1 {
        String title;
        Data1(String title) {
            this.title = title;
        }
    }


    public static void main(String[] args) {
        System.out.println(todo.title);
    }
}
