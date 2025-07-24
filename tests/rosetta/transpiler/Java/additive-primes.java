public class Main {

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (n % 2 == 0) {
            return n == 2;
        }
        if (n % 3 == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (n % d == 0) {
                return false;
            }
            d = d + 2;
            if (n % d == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static int sumDigits(int n) {
        int s = 0;
        int x = n;
        while (x > 0) {
            s = s + x % 10;
            x = ((Number)((x / 10))).intValue();
        }
        return s;
    }

    static String pad(int n) {
        if (n < 10) {
            return "  " + String.valueOf(n);
        }
        if (n < 100) {
            return " " + String.valueOf(n);
        }
        return String.valueOf(n);
    }

    static void main() {
        System.out.println("Additive primes less than 500:");
        int count = 0;
        String line = "";
        int lineCount = 0;
        int i = 2;
        while (i < 500) {
            if (isPrime(i) && isPrime(sumDigits(i))) {
                count = count + 1;
                line = line + pad(i) + "  ";
                lineCount = lineCount + 1;
                if (lineCount == 10) {
                    System.out.println(line.substring(0, line.length() - 2));
                    line = "";
                    lineCount = 0;
                }
            }
            if (i > 2) {
                i = i + 2;
            } else {
                i = i + 1;
            }
        }
        if (lineCount > 0) {
            System.out.println(line.substring(0, line.length() - 2));
        }
        System.out.println(String.valueOf(count) + " additive primes found.");
    }
    public static void main(String[] args) {
        main();
    }
}
