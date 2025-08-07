public class Main {
    static int[] DIGITS_FIFTH_POWER;

    static int digits_fifth_powers_sum(int number) {
        int total = 0;
        int n = number;
        while (n > 0) {
            int digit = Math.floorMod(n, 10);
            total = total + DIGITS_FIFTH_POWER[digit];
            n = Math.floorDiv(n, 10);
        }
        return total;
    }

    static int solution() {
        int total_1 = 0;
        int num = 1000;
        while (num < 1000000) {
            if (num == digits_fifth_powers_sum(num)) {
                total_1 = total_1 + num;
            }
            num = num + 1;
        }
        return total_1;
    }
    public static void main(String[] args) {
        DIGITS_FIFTH_POWER = ((int[])(new int[]{0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049}));
        System.out.println(solution());
    }
}
