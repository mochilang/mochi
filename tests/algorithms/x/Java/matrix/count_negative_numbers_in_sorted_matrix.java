public class Main {
    static int[][] grid;
    static int[][][] test_grids;
    static int[] results_bin = new int[0];
    static int i_4 = 0;
    static int[] results_brute = new int[0];
    static int[] results_break = new int[0];

    static int[][] generate_large_matrix() {
        int[][] result = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < 1000) {
            int[] row = ((int[])(new int[]{}));
            int j = 1000 - i;
            while (j > (-1000 - i)) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(j)).toArray()));
                j = j - 1;
            }
            result = ((int[][])(appendObj(result, row)));
            i = i + 1;
        }
        return result;
    }

    static int find_negative_index(int[] arr) {
        int left = 0;
        int right = arr.length - 1;
        if (arr.length == 0) {
            return 0;
        }
        if (arr[0] < 0) {
            return 0;
        }
        while (left <= right) {
            int mid = Math.floorDiv((left + right), 2);
            int num = arr[mid];
            if (num < 0) {
                if (mid == 0) {
                    return 0;
                }
                if (arr[mid - 1] >= 0) {
                    return mid;
                }
                right = mid - 1;
            } else {
                left = mid + 1;
            }
        }
        return arr.length;
    }

    static int count_negatives_binary_search(int[][] grid) {
        int total = 0;
        int bound = grid[0].length;
        int i_1 = 0;
        while (i_1 < grid.length) {
            int[] row_1 = ((int[])(grid[i_1]));
            int idx = find_negative_index(((int[])(java.util.Arrays.copyOfRange(row_1, 0, bound))));
            bound = idx;
            total = total + idx;
            i_1 = i_1 + 1;
        }
        return (grid.length * grid[0].length) - total;
    }

    static int count_negatives_brute_force(int[][] grid) {
        int count = 0;
        int i_2 = 0;
        while (i_2 < grid.length) {
            int[] row_2 = ((int[])(grid[i_2]));
            int j_1 = 0;
            while (j_1 < row_2.length) {
                if (row_2[j_1] < 0) {
                    count = count + 1;
                }
                j_1 = j_1 + 1;
            }
            i_2 = i_2 + 1;
        }
        return count;
    }

    static int count_negatives_brute_force_with_break(int[][] grid) {
        int total_1 = 0;
        int i_3 = 0;
        while (i_3 < grid.length) {
            int[] row_3 = ((int[])(grid[i_3]));
            int j_2 = 0;
            while (j_2 < row_3.length) {
                int number = row_3[j_2];
                if (number < 0) {
                    total_1 = total_1 + (row_3.length - j_2);
                    break;
                }
                j_2 = j_2 + 1;
            }
            i_3 = i_3 + 1;
        }
        return total_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            grid = ((int[][])(generate_large_matrix()));
            test_grids = ((int[][][])(new int[][][]{new int[][]{new int[]{4, 3, 2, -1}, new int[]{3, 2, 1, -1}, new int[]{1, 1, -1, -2}, new int[]{-1, -1, -2, -3}}, new int[][]{new int[]{3, 2}, new int[]{1, 0}}, new int[][]{new int[]{7, 7, 6}}, new int[][]{new int[]{7, 7, 6}, new int[]{-1, -2, -3}}, grid}));
            results_bin = ((int[])(new int[]{}));
            i_4 = 0;
            while (i_4 < test_grids.length) {
                results_bin = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(results_bin), java.util.stream.IntStream.of(count_negatives_binary_search(((int[][])(test_grids[i_4]))))).toArray()));
                i_4 = i_4 + 1;
            }
            System.out.println(_p(results_bin));
            results_brute = ((int[])(new int[]{}));
            i_4 = 0;
            while (i_4 < test_grids.length) {
                results_brute = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(results_brute), java.util.stream.IntStream.of(count_negatives_brute_force(((int[][])(test_grids[i_4]))))).toArray()));
                i_4 = i_4 + 1;
            }
            System.out.println(_p(results_brute));
            results_break = ((int[])(new int[]{}));
            i_4 = 0;
            while (i_4 < test_grids.length) {
                results_break = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(results_break), java.util.stream.IntStream.of(count_negatives_brute_force_with_break(((int[][])(test_grids[i_4]))))).toArray()));
                i_4 = i_4 + 1;
            }
            System.out.println(_p(results_break));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
