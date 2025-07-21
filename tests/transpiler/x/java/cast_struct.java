public class Main {
    static Todo todo = new Data1("hi");
    static class Data1 {
        String title;
        Data1(String title) {
            this.title = title;
        }
        boolean containsKey(String k) {
            if (k.equals("title")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println(((Integer) (todo.get("title"))));
    }
}
