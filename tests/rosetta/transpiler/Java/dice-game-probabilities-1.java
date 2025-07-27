public class Main {

    static int powInt(int base, int exp) {
        int r = 1;
        int b = base;
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                r = r * b;
            }
            b = b * b;
            e = e / ((Number)(2)).intValue();
        }
        return r;
    }

    static int minInt(int x, int y) {
        if (x < y) {
            return x;
        }
        return y;
    }

    static void throwDie(int nSides, int nDice, int s, int[] counts) {
        if (nDice == 0) {
counts[s] = counts[s] + 1;
            return;
        }
        int i = 1;
        while (i <= nSides) {
            throwDie(nSides, nDice - 1, s + i, counts);
            i = i + 1;
        }
    }

    static double beatingProbability(int nSides1, int nDice1, int nSides2, int nDice2) {
        int len1 = (nSides1 + 1) * nDice1;
        int[] c1 = new int[]{};
        int i = 0;
        while (i < len1) {
            c1 = java.util.stream.IntStream.concat(java.util.Arrays.stream(c1), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        throwDie(nSides1, nDice1, 0, c1);
        int len2 = (nSides2 + 1) * nDice2;
        int[] c2 = new int[]{};
        int j = 0;
        while (j < len2) {
            c2 = java.util.stream.IntStream.concat(java.util.Arrays.stream(c2), java.util.stream.IntStream.of(0)).toArray();
            j = j + 1;
        }
        throwDie(nSides2, nDice2, 0, c2);
        double p12 = (((Number)(powInt(nSides1, nDice1))).doubleValue()) * (((Number)(powInt(nSides2, nDice2))).doubleValue());
        double tot = 0.0;
        i = 0;
        while (i < len1) {
            j = 0;
            int m = minInt(i, len2);
            while (j < m) {
                tot = tot + (c1[i] * c2[j]) / p12;
                j = j + 1;
            }
            i = i + 1;
        }
        return tot;
    }
    public static void main(String[] args) {
        System.out.println(String.valueOf(beatingProbability(4, 9, 6, 6)));
        System.out.println(String.valueOf(beatingProbability(10, 5, 7, 6)));
    }
}
