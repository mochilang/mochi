public class Main {

    static int solution(int n) {
        int sum_of_squares = Math.floorDiv(n * (n + 1) * (2 * n + 1), 6);
        int sum_first_n = Math.floorDiv(n * (n + 1), 2);
        int square_of_sum = sum_first_n * sum_first_n;
        return square_of_sum - sum_of_squares;
    }
    public static void main(String[] args) {
        System.out.println(solution(10));
        System.out.println(solution(15));
        System.out.println(solution(20));
        System.out.println(solution(50));
        System.out.println(solution(100));
    }
}
