public class Main {

    static int solution(int limit) {
        int[] phi = ((int[])(new int[]{}));
        int i = 0;
        while (i <= limit) {
            phi = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(phi), java.util.stream.IntStream.of(i)).toArray()));
            i = i + 1;
        }
        int n = 2;
        while (n <= limit) {
            if (phi[n] == n) {
                int k = n;
                while (k <= limit) {
phi[k] = phi[k] - Math.floorDiv(phi[k], n);
                    k = k + n;
                }
            }
            n = n + 1;
        }
        int total = 0;
        int m = 2;
        while (m <= limit) {
            total = total + phi[m];
            m = m + 1;
        }
        return total;
    }

    static void main() {
        System.out.println(solution(8));
        System.out.println(solution(1000));
    }
    public static void main(String[] args) {
        main();
    }
}
