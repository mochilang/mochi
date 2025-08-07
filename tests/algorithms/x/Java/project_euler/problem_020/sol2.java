public class Main {

    static int factorial(int n) {
        int result = 1;
        int i = 2;
        while (i <= n) {
            result = result * i;
            i = i + 1;
        }
        return result;
    }

    static int digit_sum(int n) {
        int total = 0;
        int m = n;
        while (m > 0) {
            total = total + (Math.floorMod(m, 10));
            m = Math.floorDiv(m, 10);
        }
        return total;
    }

    static int solution(int num) {
        int f = factorial(num);
        return digit_sum(f);
    }

    static void main() {
        System.out.println(solution(100));
        System.out.println(solution(50));
        System.out.println(solution(10));
        System.out.println(solution(5));
        System.out.println(solution(3));
        System.out.println(solution(2));
        System.out.println(solution(1));
    }
    public static void main(String[] args) {
        main();
    }
}
