public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static void main() {
        while (true) {
            String line = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            java.math.BigInteger n = new java.math.BigInteger(String.valueOf(Integer.parseInt(line)));
            if (n.compareTo(java.math.BigInteger.valueOf(42)) == 0) {
                break;
            }
            System.out.println(line);
        }
    }
    public static void main(String[] args) {
        main();
    }
}
