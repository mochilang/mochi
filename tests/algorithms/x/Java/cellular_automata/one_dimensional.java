public class Main {
    static int[] initial;
    static int[][] cells = new int[0][];
    static int[] rules;
    static int time = 0;
    static int t = 0;

    static int[] format_ruleset(int ruleset) {
        int rs = ruleset;
        int[] bits_rev = ((int[])(new int[]{}));
        int i = 0;
        while (i < 8) {
            bits_rev = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bits_rev), java.util.stream.IntStream.of(Math.floorMod(rs, 2))).toArray()));
            rs = rs / 2;
            i = i + 1;
        }
        int[] bits = ((int[])(new int[]{}));
        int j = bits_rev.length - 1;
        while (j >= 0) {
            bits = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(bits), java.util.stream.IntStream.of(bits_rev[j])).toArray()));
            j = j - 1;
        }
        return bits;
    }

    static int[] new_generation(int[][] cells, int[] rules, int time) {
        int population = cells[0].length;
        int[] next_generation = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < population) {
            int left_neighbor = i_1 == 0 ? 0 : cells[time][i_1 - 1];
            int right_neighbor = i_1 == population - 1 ? 0 : cells[time][i_1 + 1];
            int center = cells[time][i_1];
            int idx = 7 - (left_neighbor * 4 + center * 2 + right_neighbor);
            next_generation = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(next_generation), java.util.stream.IntStream.of(rules[idx])).toArray()));
            i_1 = i_1 + 1;
        }
        return next_generation;
    }

    static String cells_to_string(int[] row) {
        String result = "";
        int i_2 = 0;
        while (i_2 < row.length) {
            if (row[i_2] == 1) {
                result = result + "#";
            } else {
                result = result + ".";
            }
            i_2 = i_2 + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        initial = ((int[])(new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
        cells = ((int[][])(new int[][]{initial}));
        rules = ((int[])(format_ruleset(30)));
        time = 0;
        while (time < 16) {
            int[] next = ((int[])(new_generation(((int[][])(cells)), ((int[])(rules)), time)));
            cells = ((int[][])(appendObj(cells, next)));
            time = time + 1;
        }
        t = 0;
        while (t < cells.length) {
            System.out.println(cells_to_string(((int[])(cells[t]))));
            t = t + 1;
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
