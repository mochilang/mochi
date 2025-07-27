public class Main {

    static void main() {
        int[][] rows = new int[][]{};
        for (int i = 0; i < 4; i++) {
            rows = appendObj(rows, new int[]{i * 3, i * 3 + 1, i * 3 + 2});
        }
        System.out.println("<table>");
        System.out.println("    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>");
        int idx = 0;
        for (int[] row : rows) {
            System.out.println("    <tr><td>" + String.valueOf(idx) + "</td><td>" + String.valueOf(row[0]) + "</td><td>" + String.valueOf(row[1]) + "</td><td>" + String.valueOf(row[2]) + "</td></tr>");
            idx = idx + 1;
        }
        System.out.println("</table>");
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
