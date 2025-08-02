public class Main {

    static void printFactors(int n) {
        if (n < 1) {
            System.out.println("\nFactors of " + (String)(_p(n)) + " not computed");
            return;
        }
        System.out.println("\nFactors of " + (String)(_p(n)) + ": ");
        int[][] fs = new int[1][];
        fs[0] = new int[]{1};
        java.util.function.BiConsumer<Integer,Integer> apf = (p, e) -> {
        int orig = fs[0].length;
        int pp = p;
        int i = 0;
        while (i < e) {
            int j = 0;
            while (j < orig) {
                fs[0] = java.util.stream.IntStream.concat(java.util.Arrays.stream(fs[0]), java.util.stream.IntStream.of(fs[0][j] * pp)).toArray();
                j = j + 1;
            }
            i = i + 1;
            pp = pp * p;
        }
};
        int e_1 = 0;
        int m = n;
        while (Math.floorMod(m, 2) == 0) {
            m = ((Number)((m / 2))).intValue();
            e_1 = e_1 + 1;
        }
        apf.accept(2, e_1);
        int d = 3;
        while (m > 1) {
            if (d * d > m) {
                d = m;
            }
            e_1 = 0;
            while (Math.floorMod(m, d) == 0) {
                m = ((Number)((m / d))).intValue();
                e_1 = e_1 + 1;
            }
            if (e_1 > 0) {
                apf.accept(d, e_1);
            }
            d = d + 2;
        }
        System.out.println(_p(fs[0]));
        System.out.println("Number of factors = " + (String)(_p(fs[0].length)));
    }
    public static void main(String[] args) {
        printFactors(-1);
        printFactors(0);
        printFactors(1);
        printFactors(2);
        printFactors(3);
        printFactors(53);
        printFactors(45);
        printFactors(64);
        printFactors((int)600851475143L);
        printFactors((int)999999999999999989L);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
