public class Main {

    static int countDivisors(int n) {
        if (n < 2) {
            return 1;
        }
        int count = 2;
        int i = 2;
        while (i <= n / 2) {
            if (n % i == 0) {
                count = count + 1;
            }
            i = i + 1;
        }
        return count;
    }

    static void main() {
        System.out.println("The first 20 anti-primes are:");
        int maxDiv = 0;
        int count = 0;
        int n = 1;
        String line = "";
        while (count < 20) {
            int d = countDivisors(n);
            if (d > maxDiv) {
                line = String.valueOf(String.valueOf(line + String.valueOf(n)) + " ");
                maxDiv = d;
                count = count + 1;
            }
            n = n + 1;
        }
        line = line.substring(0, line.length() - 1);
        System.out.println(line);
    }
    public static void main(String[] args) {
        main();
    }
}
