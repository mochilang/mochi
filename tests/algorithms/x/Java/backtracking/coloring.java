public class Main {
    static int[][] graph;

    static boolean valid_coloring(int[] neighbours, int[] colored_vertices, int color) {
        int i = 0;
        while (i < neighbours.length) {
            if (neighbours[i] == 1 && colored_vertices[i] == color) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static boolean util_color(int[][] graph, int max_colors, int[] colored_vertices, int index) {
        if (index == graph.length) {
            return true;
        }
        int c = 0;
        while (c < max_colors) {
            if (((Boolean)(valid_coloring(((int[])(graph[index])), ((int[])(colored_vertices)), c)))) {
colored_vertices[index] = c;
                if (((Boolean)(util_color(((int[][])(graph)), max_colors, ((int[])(colored_vertices)), index + 1)))) {
                    return true;
                }
colored_vertices[index] = -1;
            }
            c = c + 1;
        }
        return false;
    }

    static int[] color(int[][] graph, int max_colors) {
        int[] colored_vertices = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < graph.length) {
            colored_vertices = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(colored_vertices), java.util.stream.IntStream.of(-1)).toArray()));
            i_1 = i_1 + 1;
        }
        if (((Boolean)(util_color(((int[][])(graph)), max_colors, ((int[])(colored_vertices)), 0)))) {
            return colored_vertices;
        }
        return new int[]{};
    }
    public static void main(String[] args) {
        graph = ((int[][])(new int[][]{new int[]{0, 1, 0, 0, 0}, new int[]{1, 0, 1, 0, 1}, new int[]{0, 1, 0, 1, 0}, new int[]{0, 1, 1, 0, 0}, new int[]{0, 1, 0, 0, 0}}));
        System.out.println(color(((int[][])(graph)), 3));
        System.out.println("\n");
        System.out.println(color(((int[][])(graph)), 2).length);
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
