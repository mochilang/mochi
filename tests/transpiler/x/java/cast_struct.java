public class Main {
    static Todo todo = new Data1("hi");
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
