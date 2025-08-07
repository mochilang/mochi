public class Main {

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int temp = Math.floorMod(x, y);
            x = y;
            y = temp;
        }
        return x;
    }

    static int solution(int max_d) {
        int fractions_number = 0;
        int d = 0;
        while (d <= max_d) {
            int n = Math.floorDiv(d, 3) + 1;
            int half = Math.floorDiv((d + 1), 2);
            while (n < half) {
                if (gcd(n, d) == 1) {
                    fractions_number = fractions_number + 1;
                }
                n = n + 1;
            }
            d = d + 1;
        }
        return fractions_number;
    }

    static void main() {
        System.out.println(solution(12000));
    }
    public static void main(String[] args) {
        main();
    }
}
