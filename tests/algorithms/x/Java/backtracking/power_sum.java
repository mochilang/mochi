public class Main {

    static int int_pow(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static int backtrack(int target, int exp, int current, int current_sum) {
        if (current_sum == target) {
            return 1;
        }
        int p = int_pow(current, exp);
        int count = 0;
        if (current_sum + p <= target) {
            count = count + backtrack(target, exp, current + 1, current_sum + p);
        }
        if (p < target) {
            count = count + backtrack(target, exp, current + 1, current_sum);
        }
        return count;
    }

    static int solve(int target, int exp) {
        if (!(1 <= target && target <= 1000 && 2 <= exp && exp <= 10)) {
            System.out.println("Invalid input");
            return 0;
        }
        return backtrack(target, exp, 1, 0);
    }
    public static void main(String[] args) {
        System.out.println(solve(13, 2));
        System.out.println(solve(10, 2));
        System.out.println(solve(10, 3));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
    }
}
