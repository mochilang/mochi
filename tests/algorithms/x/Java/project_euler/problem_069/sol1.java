public class Main {

    static boolean is_prime(int n) {
        if (n < 2) {
            return false;
        }
        int i = 2;
        while (i * i <= n) {
            if (Math.floorMod(n, i) == 0) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static int solution(int limit) {
        if (limit < 2) {
            return 1;
        }
        int result = 1;
        int p = 2;
        while (result * p <= limit) {
            if (((Boolean)(is_prime(p)))) {
                result = result * p;
            }
            p = p + 1;
        }
        return result;
    }

    static void main() {
        int ans = solution(1000000);
        System.out.println(ans);
    }
    public static void main(String[] args) {
        main();
    }
}
