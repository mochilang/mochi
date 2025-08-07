public class Main {

    static int sum_digits(int num) {
        int n = num;
        int digit_sum = 0;
        while (n > 0) {
            digit_sum = digit_sum + Math.floorMod(n, 10);
            n = Math.floorDiv(n, 10);
        }
        return digit_sum;
    }

    static int solution(int max_n) {
        int pre_numerator = 1;
        int cur_numerator = 2;
        int i = 2;
        while (i <= max_n) {
            int temp = pre_numerator;
            int e_cont = 1;
            if (Math.floorMod(i, 3) == 0) {
                e_cont = Math.floorDiv(2 * i, 3);
            }
            pre_numerator = cur_numerator;
            cur_numerator = e_cont * pre_numerator + temp;
            i = i + 1;
        }
        return sum_digits(cur_numerator);
    }
    public static void main(String[] args) {
        System.out.println(solution(9));
        System.out.println(solution(10));
        System.out.println(solution(50));
        System.out.println(solution(100));
    }
}
