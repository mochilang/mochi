public class Main {

    static void main() {
        int n = 1;
        while (n <= 51300) {
            if (Math.floorMod(n, 100) == 0) {
                System.out.println(String.valueOf(n));
            }
            n = n + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
