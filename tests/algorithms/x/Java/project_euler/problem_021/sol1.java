public class Main {

    static int int_sqrt(int n) {
        int r = 0;
        while ((r + 1) * (r + 1) <= n) {
            r = r + 1;
        }
        return r;
    }

    static int sum_of_divisors(int n) {
        int total = 0;
        int root = int_sqrt(n);
        int i = 1;
        while (i <= root) {
            if (Math.floorMod(n, i) == 0) {
                if (i * i == n) {
                    total = total + i;
                } else {
                    total = total + i + ((Number)((Math.floorDiv(n, i)))).intValue();
                }
            }
            i = i + 1;
        }
        return total - n;
    }

    static int solution(int limit) {
        int total_1 = 0;
        int i_1 = 1;
        while (i_1 < limit) {
            int s = sum_of_divisors(i_1);
            if (s != i_1 && sum_of_divisors(s) == i_1) {
                total_1 = total_1 + i_1;
            }
            i_1 = i_1 + 1;
        }
        return total_1;
    }
    public static void main(String[] args) {
        System.out.println(solution(10000));
        System.out.println(solution(5000));
        System.out.println(solution(1000));
        System.out.println(solution(100));
        System.out.println(solution(50));
    }
}
