public class BreakContinue {
    public static void main(String[] args) {
        int[] numbers = {1,2,3,4,5,6,7,8,9};
        for (int n : numbers) {
            if (n % 2 == 0) {
                continue;
            }
            if (n > 7) {
                break;
            }
            System.out.println("odd number: " + n);
        }
    }
}
