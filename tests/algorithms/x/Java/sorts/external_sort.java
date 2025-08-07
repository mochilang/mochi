public class Main {

    static int[] subarray(int[] xs, int start, int end) {
        int[] result = ((int[])(new int[]{}));
        int k = start;
        while (k < end) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(xs[k])).toArray()));
            k = k + 1;
        }
        return result;
    }

    static int[] merge(int[] left_half, int[] right_half) {
        int[] result_1 = ((int[])(new int[]{}));
        int i = 0;
        int j = 0;
        while (i < left_half.length && j < right_half.length) {
            if (left_half[i] < right_half[j]) {
                result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(left_half[i])).toArray()));
                i = i + 1;
            } else {
                result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(right_half[j])).toArray()));
                j = j + 1;
            }
        }
        while (i < left_half.length) {
            result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(left_half[i])).toArray()));
            i = i + 1;
        }
        while (j < right_half.length) {
            result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(right_half[j])).toArray()));
            j = j + 1;
        }
        return result_1;
    }

    static int[] merge_sort(int[] array) {
        if (array.length <= 1) {
            return array;
        }
        int middle = Math.floorDiv(array.length, 2);
        int[] left_half = ((int[])(subarray(((int[])(array)), 0, middle)));
        int[] right_half = ((int[])(subarray(((int[])(array)), middle, array.length)));
        int[] sorted_left = ((int[])(merge_sort(((int[])(left_half)))));
        int[] sorted_right = ((int[])(merge_sort(((int[])(right_half)))));
        return merge(((int[])(sorted_left)), ((int[])(sorted_right)));
    }

    static int[][] split_into_blocks(int[] data, int block_size) {
        int[][] blocks = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < data.length) {
            int end = i_1 + block_size < data.length ? i_1 + block_size : data.length;
            int[] block = ((int[])(subarray(((int[])(data)), i_1, end)));
            int[] sorted_block = ((int[])(merge_sort(((int[])(block)))));
            blocks = ((int[][])(appendObj(blocks, sorted_block)));
            i_1 = end;
        }
        return blocks;
    }

    static int[] merge_blocks(int[][] blocks) {
        int num_blocks = blocks.length;
        int[] indices = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < num_blocks) {
            indices = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(indices), java.util.stream.IntStream.of(0)).toArray()));
            i_2 = i_2 + 1;
        }
        int[] result_2 = ((int[])(new int[]{}));
        boolean done = false;
        while (!done) {
            done = true;
            int min_val = 0;
            int min_block = 0 - 1;
            int j_1 = 0;
            while (j_1 < num_blocks) {
                int idx = indices[j_1];
                if (idx < blocks[j_1].length) {
                    int val = blocks[j_1][idx];
                    if (min_block == (0 - 1) || val < min_val) {
                        min_val = val;
                        min_block = j_1;
                    }
                    done = false;
                }
                j_1 = j_1 + 1;
            }
            if (!done) {
                result_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_2), java.util.stream.IntStream.of(min_val)).toArray()));
indices[min_block] = indices[min_block] + 1;
            }
        }
        return result_2;
    }

    static int[] external_sort(int[] data, int block_size) {
        int[][] blocks_1 = ((int[][])(split_into_blocks(((int[])(data)), block_size)));
        return merge_blocks(((int[][])(blocks_1)));
    }

    static void main() {
        int[] data = ((int[])(new int[]{7, 1, 5, 3, 9, 2, 6, 4, 8, 0}));
        int[] sorted_data = ((int[])(external_sort(((int[])(data)), 3)));
        System.out.println(java.util.Arrays.toString(sorted_data));
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
