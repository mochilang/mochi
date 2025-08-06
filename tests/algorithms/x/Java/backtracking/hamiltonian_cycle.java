public class Main {
    static boolean valid_connection(int[][] graph, int next_ver, int curr_ind, int[] path) {
        if (graph[path[curr_ind - 1]][next_ver] == 0) {
            return false;
        }
        for (int v : path) {
            if (v == next_ver) {
                return false;
            }
        }
        return true;
    }

    static boolean util_hamilton_cycle(int[][] graph, int[] path, int curr_ind) {
        if (curr_ind == graph.length) {
            return graph[path[curr_ind - 1]][path[0]] == 1;
        }
        int next_ver = 0;
        while (next_ver < graph.length) {
            if (((Boolean)(valid_connection(((int[][])(graph)), next_ver, curr_ind, ((int[])(path)))))) {
path[curr_ind] = next_ver;
                if (((Boolean)(util_hamilton_cycle(((int[][])(graph)), ((int[])(path)), curr_ind + 1)))) {
                    return true;
                }
path[curr_ind] = -1;
            }
            next_ver = next_ver + 1;
        }
        return false;
    }

    static int[] hamilton_cycle(int[][] graph, int start_index) {
        int[] path = new int[0];
        int i = 0;
        while (i < graph.length + 1) {
path[i] = -1;
            i = i + 1;
        }
path[0] = start_index;
        int last = path.length - 1;
path[last] = start_index;
        if (((Boolean)(util_hamilton_cycle(((int[][])(graph)), ((int[])(path)), 1)))) {
            return path;
        }
        return new int[]{};
    }
    public static void main(String[] args) {
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
