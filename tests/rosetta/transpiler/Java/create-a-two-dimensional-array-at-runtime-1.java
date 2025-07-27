public class Main {

    static void main() {
        int row = 3;
        int col = 4;
        int[][] a = new int[][]{};
        int i = 0;
        while (i < row) {
            int[] rowArr = new int[]{};
            int j = 0;
            while (j < col) {
                rowArr = java.util.stream.IntStream.concat(java.util.Arrays.stream(rowArr), java.util.stream.IntStream.of(0)).toArray();
                j = j + 1;
            }
            a = appendObj(a, rowArr);
            i = i + 1;
        }
        System.out.println("a[0][0] = " + String.valueOf(a[0][0]));
a[((Number)((row - 1))).intValue()][((Number)((col - 1))).intValue()] = 7;
        System.out.println("a[" + String.valueOf(row - 1) + "][" + String.valueOf(col - 1) + "] = " + String.valueOf(a[((Number)((row - 1))).intValue()][((Number)((col - 1))).intValue()]));
        a = null;
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
