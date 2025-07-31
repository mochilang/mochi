class Main {
    class Todo {
        static String title;
        Todo(String title) {
            this.title = title;
        }
    }
    static Todo todo = new Todo("hi");
    class Data1 {
        static String title;
        Data1(String title) {
            this.title = title;
        }
    }
    public static void main(String[] args) {
        System.out.println(todo.title);
    }
}
