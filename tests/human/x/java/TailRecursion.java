public class TailRecursion {
    static int sumRec(int n, int acc) {
        if (n == 0) {
            return acc;
        }
        return sumRec(n - 1, acc + n);
    }

    public static void main(String[] args) {
        System.out.println(sumRec(10, 0));
    }
}
