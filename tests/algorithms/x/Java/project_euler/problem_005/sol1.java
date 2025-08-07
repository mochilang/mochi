public class Main {

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static int lcm(int a, int b) {
        return Math.floorDiv(a, gcd(a, b)) * b;
    }

    static int solution(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Parameter n must be greater than or equal to one."));
        }
        int result = 1;
        int i = 2;
        while (i <= n) {
            result = lcm(result, i);
            i = i + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        System.out.println(solution(10));
        System.out.println(solution(15));
        System.out.println(solution(22));
        System.out.println(solution(20));
    }
}
