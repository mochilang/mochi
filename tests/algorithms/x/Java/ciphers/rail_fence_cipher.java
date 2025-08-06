public class Main {
    static java.util.Map<Integer,String> bf;

    static String encrypt(String input_string, int key) {
        if (key <= 0) {
            throw new RuntimeException(String.valueOf("Height of grid can't be 0 or negative"));
        }
        if (key == 1 || _runeLen(input_string) <= key) {
            return input_string;
        }
        int lowest = key - 1;
        String[][] temp_grid = ((String[][])(new String[][]{}));
        int i = 0;
        while (i < key) {
            temp_grid = ((String[][])(appendObj(temp_grid, new String[]{})));
            i = i + 1;
        }
        int position = 0;
        while (position < _runeLen(input_string)) {
            int num = Math.floorMod(position, (lowest * 2));
            int alt = lowest * 2 - num;
            if (num > alt) {
                num = alt;
            }
            String[] row = ((String[])(temp_grid[num]));
            row = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(_substr(input_string, position, position + 1))).toArray(String[]::new)));
temp_grid[num] = ((String[])(row));
            position = position + 1;
        }
        String output = "";
        i = 0;
        while (i < key) {
            String[] row_1 = ((String[])(temp_grid[i]));
            int j = 0;
            while (j < row_1.length) {
                output = output + row_1[j];
                j = j + 1;
            }
            i = i + 1;
        }
        return output;
    }

    static String decrypt(String input_string, int key) {
        if (key <= 0) {
            throw new RuntimeException(String.valueOf("Height of grid can't be 0 or negative"));
        }
        if (key == 1) {
            return input_string;
        }
        int lowest_1 = key - 1;
        int[] counts = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < key) {
            counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        int pos = 0;
        while (pos < _runeLen(input_string)) {
            int num_1 = Math.floorMod(pos, (lowest_1 * 2));
            int alt_1 = lowest_1 * 2 - num_1;
            if (num_1 > alt_1) {
                num_1 = alt_1;
            }
counts[num_1] = counts[num_1] + 1;
            pos = pos + 1;
        }
        String[][] grid = ((String[][])(new String[][]{}));
        int counter = 0;
        i_1 = 0;
        while (i_1 < key) {
            int length = counts[i_1];
            String slice = _substr(input_string, counter, counter + length);
            String[] row_2 = ((String[])(new String[]{}));
            int j_1 = 0;
            while (j_1 < _runeLen(slice)) {
                row_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_2), java.util.stream.Stream.of(slice.substring(j_1, j_1+1))).toArray(String[]::new)));
                j_1 = j_1 + 1;
            }
            grid = ((String[][])(appendObj(grid, row_2)));
            counter = counter + length;
            i_1 = i_1 + 1;
        }
        int[] indices = ((int[])(new int[]{}));
        i_1 = 0;
        while (i_1 < key) {
            indices = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(indices), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        String output_1 = "";
        pos = 0;
        while (pos < _runeLen(input_string)) {
            int num_2 = Math.floorMod(pos, (lowest_1 * 2));
            int alt_2 = lowest_1 * 2 - num_2;
            if (num_2 > alt_2) {
                num_2 = alt_2;
            }
            output_1 = output_1 + grid[num_2][indices[num_2]];
indices[num_2] = indices[num_2] + 1;
            pos = pos + 1;
        }
        return output_1;
    }

    static java.util.Map<Integer,String> bruteforce(String input_string) {
        java.util.Map<Integer,String> results = ((java.util.Map<Integer,String>)(new java.util.LinkedHashMap<Integer, String>()));
        int key_guess = 1;
        while (key_guess < _runeLen(input_string)) {
results.put(key_guess, String.valueOf(decrypt(input_string, key_guess)));
            key_guess = key_guess + 1;
        }
        return results;
    }
    public static void main(String[] args) {
        System.out.println(encrypt("Hello World", 4));
        System.out.println(decrypt("HWe olordll", 4));
        bf = bruteforce("HWe olordll");
        System.out.println(((String)(bf).get(4)));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
