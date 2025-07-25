public class Main {

    static int intSqrt(int x) {
        if (x < 2) {
            return x;
        }
        int left = 1;
        int right = x / 2;
        int ans = 0;
        while (left <= right) {
            int mid = left + (right - left) / 2;
            int sq = mid * mid;
            if (sq == x) {
                return mid;
            }
            if (sq < x) {
                left = mid + 1;
                ans = mid;
            } else {
                right = mid - 1;
            }
        }
        return ans;
    }

    static int sumRecip(int n) {
        int s = 1;
        int limit = intSqrt(n);
        int f = 2;
        while (f <= limit) {
            if (n % f == 0) {
                s = s + n / f;
                int f2 = n / f;
                if (f2 != f) {
                    s = s + f;
                }
            }
            f = f + 1;
        }
        return s;
    }

    static void main() {
        int[] nums = new int[]{6, 28, 120, 496, 672, 8128, 30240, 32760, 523776};
        for (int n : nums) {
            int s = sumRecip(n);
            if (s % n == 0) {
                int val = s / n;
                String perfect = "";
                if (val == 1) {
                    perfect = "perfect!";
                }
                System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf("Sum of recipr. factors of " + String.valueOf(n)) + " = ") + String.valueOf(val)) + " exactly ") + perfect);
            }
        }
    }
    public static void main(String[] args) {
        main();
    }
}
