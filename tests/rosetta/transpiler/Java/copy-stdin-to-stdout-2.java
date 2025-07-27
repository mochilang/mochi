public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static void main() {
        while (true) {
            String line = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            if ((line.equals(""))) {
                break;
            }
            System.out.println(line);
        }
    }
    public static void main(String[] args) {
        main();
    }
}
