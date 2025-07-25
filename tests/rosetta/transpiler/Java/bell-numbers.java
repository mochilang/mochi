public class Main {

    static java.math.BigInteger[][] bellTriangle(int n) {
        java.math.BigInteger[][] tri = new java.math.BigInteger[][]{};
        int i = 0;
        while (i < n) {
            java.math.BigInteger[] row = new java.math.BigInteger[]{};
            int j = 0;
            while (j < i) {
                row = java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new);
                j = j + 1;
            }
            tri = appendObj(tri, row);
            i = i + 1;
        }
tri[1][0] = 1;
        i = 2;
        while (i < n) {
tri[i][0] = tri[i - 1][i - 2];
            int j = 1;
            while (j < i) {
tri[i][j] = ((Number)(tri[i][j - 1])).intValue() + ((Number)(tri[i - 1][j - 1])).intValue();
                j = j + 1;
            }
            i = i + 1;
        }
        return tri;
    }

    static void main() {
        java.math.BigInteger[][] bt = bellTriangle(51);
        System.out.println("First fifteen and fiftieth Bell numbers:");
        for (int i = 1; i < 16; i++) {
            System.out.println(String.valueOf(String.valueOf("" + String.valueOf(_padStart(String.valueOf(i), 2, " "))) + ": ") + String.valueOf(bt[i][0]));
        }
        System.out.println("50: " + String.valueOf(bt[50][0]));
        System.out.println("");
        System.out.println("The first ten rows of Bell's triangle:");
        for (int i = 1; i < 11; i++) {
            System.out.println(java.util.Arrays.toString(bt[i]));
        }
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _padStart(String s, int width, String pad) {
        String out = s;
        while (out.length() < width) { out = pad + out; }
        return out;
    }
}
