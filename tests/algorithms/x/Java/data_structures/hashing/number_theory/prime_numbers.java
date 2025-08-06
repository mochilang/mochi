public class Main {

    static boolean isPrime(int number) {
        if (number < 2) {
            return false;
        }
        if (number < 4) {
            return true;
        }
        if (Math.floorMod(number, 2) == 0) {
            return false;
        }
        int i = 3;
        while (i * i <= number) {
            if (Math.floorMod(number, i) == 0) {
                return false;
            }
            i = i + 2;
        }
        return true;
    }

    static int nextPrime(int value, int factor, boolean desc) {
        int v = value * factor;
        int firstValue = v;
        while (!(Boolean)isPrime(v)) {
            if (((Boolean)(desc))) {
                v = v - 1;
            } else {
                v = v + 1;
            }
        }
        if (v == firstValue) {
            if (((Boolean)(desc))) {
                return nextPrime(v - 1, 1, desc);
            } else {
                return nextPrime(v + 1, 1, desc);
            }
        }
        return v;
    }
    public static void main(String[] args) {
        System.out.println(isPrime(0));
        System.out.println(isPrime(1));
        System.out.println(isPrime(2));
        System.out.println(isPrime(3));
        System.out.println(isPrime(27));
        System.out.println(isPrime(87));
        System.out.println(isPrime(563));
        System.out.println(isPrime(2999));
        System.out.println(isPrime(67483));
        System.out.println(nextPrime(14, 1, false));
        System.out.println(nextPrime(14, 1, true));
    }
}
