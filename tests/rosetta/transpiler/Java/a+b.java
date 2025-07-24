public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static void main() {
        int a = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        int b = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        System.out.println(a + b);
    }
    public static void main(String[] args) {
        main();
    }
}
