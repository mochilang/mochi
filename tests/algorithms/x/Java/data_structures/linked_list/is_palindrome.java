public class Main {

    static boolean is_palindrome(int[] values) {
        int[] stack = ((int[])(new int[]{}));
        int fast = 0;
        int slow = 0;
        int n = values.length;
        while (fast < n && fast + 1 < n) {
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(values[slow])).toArray()));
            slow = slow + 1;
            fast = fast + 2;
        }
        if (fast == n - 1) {
            slow = slow + 1;
        }
        int i = stack.length - 1;
        while (slow < n) {
            if (stack[i] != values[slow]) {
                return false;
            }
            i = i - 1;
            slow = slow + 1;
        }
        return true;
    }

    static void main() {
        System.out.println(is_palindrome(((int[])(new int[]{}))));
        System.out.println(is_palindrome(((int[])(new int[]{1}))));
        System.out.println(is_palindrome(((int[])(new int[]{1, 2}))));
        System.out.println(is_palindrome(((int[])(new int[]{1, 2, 1}))));
        System.out.println(is_palindrome(((int[])(new int[]{1, 2, 2, 1}))));
    }
    public static void main(String[] args) {
        main();
    }
}
