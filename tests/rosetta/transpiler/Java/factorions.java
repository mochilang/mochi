public class Main {
    static int[] facts;
    static int n;

    public static void main(String[] args) {
        facts = new int[]{1};
        n = 1;
        while (n < 12) {
            facts = java.util.stream.IntStream.concat(java.util.Arrays.stream(facts), java.util.stream.IntStream.of(facts[n - 1] * n)).toArray();
            n = n + 1;
        }
        for (int b = 9; b < 13; b++) {
            System.out.println("The factorions for base " + (String)(_p(b)) + " are:");
            String line = "";
            int i = 1;
            while (i < 1500000) {
                int m = i;
                int sum = 0;
                while (m > 0) {
                    int d = Math.floorMod(m, b);
                    sum = sum + facts[d];
                    m = m / b;
                }
                if (sum == i) {
                    line = line + (String)(_p(i)) + " ";
                }
                i = i + 1;
            }
            System.out.println(line);
            System.out.println("");
        }
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
