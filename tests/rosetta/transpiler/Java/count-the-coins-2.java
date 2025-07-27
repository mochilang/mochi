public class Main {
    static int amount = 1000;

    static int countChange(int amount) {
        int[] ways = new int[]{};
        int i = 0;
        while (i <= amount) {
            ways = java.util.stream.IntStream.concat(java.util.Arrays.stream(ways), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
ways[0] = 1;
        for (int coin : new int[]{100, 50, 25, 10, 5, 1}) {
            int j = coin;
            while (j <= amount) {
ways[j] = ways[j] + ways[j - coin];
                j = j + 1;
            }
        }
        return ways[amount];
    }
    public static void main(String[] args) {
        System.out.println("amount, ways to make change: " + String.valueOf(amount) + " " + String.valueOf(countChange(amount)));
    }
}
