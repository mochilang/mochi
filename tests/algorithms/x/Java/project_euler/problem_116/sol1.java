public class Main {

    static int solution(int length) {
        int[][] ways = ((int[][])(new int[][]{}));
        int i = 0;
        while (i <= length) {
            int[] row = ((int[])(new int[]{}));
            row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
            row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
            row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
            ways = ((int[][])(appendObj(ways, row)));
            i = i + 1;
        }
        int row_length = 0;
        while (row_length <= length) {
            int tile_length = 2;
            while (tile_length <= 4) {
                int tile_start = 0;
                while (tile_start <= row_length - tile_length) {
                    int remaining = row_length - tile_start - tile_length;
ways[row_length][tile_length - 2] = ways[row_length][tile_length - 2] + ways[remaining][tile_length - 2] + 1;
                    tile_start = tile_start + 1;
                }
                tile_length = tile_length + 1;
            }
            row_length = row_length + 1;
        }
        int total = 0;
        int j = 0;
        while (j < 3) {
            total = total + ways[length][j];
            j = j + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        System.out.println(solution(5));
        System.out.println(solution(50));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
