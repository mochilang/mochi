public class Main {

    static int solution(int pence) {
        int[] coins = ((int[])(new int[]{1, 2, 5, 10, 20, 50, 100, 200}));
        int[] ways = ((int[])(new int[]{}));
        int i = 0;
        while (i <= pence) {
            ways = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ways), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
ways[0] = 1;
        int idx = 0;
        while (idx < coins.length) {
            int coin = coins[idx];
            int j = coin;
            while (j <= pence) {
ways[j] = ways[j] + ways[j - coin];
                j = j + 1;
            }
            idx = idx + 1;
        }
        return ways[pence];
    }
    public static void main(String[] args) {
        System.out.println(solution(500));
        System.out.println(solution(200));
        System.out.println(solution(50));
        System.out.println(solution(10));
    }
}
