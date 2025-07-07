public class SumBuiltin {
    public static void main(String[] args) {
        int[] numbers = {1, 2, 3};
        int sum = 0;
        for (int n : numbers) {
            sum += n;
        }
        System.out.println(sum);
    }
}
