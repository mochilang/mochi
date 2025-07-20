public class Main {
    static Book book = new Book("Go", new Person("Bob", 42));

    public static void main(String[] args) {
        System.out.println(book.author.name);
    }
}
