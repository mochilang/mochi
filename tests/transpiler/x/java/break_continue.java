public class Main {
    static int[] numbers = new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9};
    public static void main(String[] args) {
        for (var n : numbers) {
            if (n % 2 == 0) {
                continue;
            }
            if (n > 7) {
                break;
            }
            System.out.println("odd number:" + n);
        }
    }
}
