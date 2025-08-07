public class Main {
    static String[] MATRIX_2;
    static int result;

    static int[] parse_row(String row_str) {
        int[] nums = ((int[])(new int[]{}));
        int current = 0;
        boolean has_digit = false;
        int i = 0;
        while (i < _runeLen(row_str)) {
            String ch = _substr(row_str, i, i + 1);
            if ((ch.equals(" "))) {
                if (has_digit) {
                    nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(nums), java.util.stream.IntStream.of(current)).toArray()));
                    current = 0;
                    has_digit = false;
                }
            } else {
                current = current * 10 + (Integer.parseInt(ch));
                has_digit = true;
            }
            i = i + 1;
        }
        if (has_digit) {
            nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(nums), java.util.stream.IntStream.of(current)).toArray()));
        }
        return nums;
    }

    static int[][] parse_matrix(String[] matrix_str) {
        int[][] matrix = ((int[][])(new int[][]{}));
        for (String row_str : matrix_str) {
            int[] row = ((int[])(parse_row(row_str)));
            matrix = ((int[][])(appendObj(matrix, row)));
        }
        return matrix;
    }

    static int bitcount(int x) {
        int count = 0;
        int y = x;
        while (y > 0) {
            if (Math.floorMod(y, 2) == 1) {
                count = count + 1;
            }
            y = Math.floorDiv(y, 2);
        }
        return count;
    }

    static int[] build_powers(int n) {
        int[] powers = ((int[])(new int[]{}));
        int i_1 = 0;
        int current_1 = 1;
        while (i_1 <= n) {
            powers = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(powers), java.util.stream.IntStream.of(current_1)).toArray()));
            current_1 = current_1 * 2;
            i_1 = i_1 + 1;
        }
        return powers;
    }

    static int solution(String[] matrix_str) {
        int[][] arr = ((int[][])(parse_matrix(((String[])(matrix_str)))));
        int n = arr.length;
        int[] powers_1 = ((int[])(build_powers(n)));
        int size = powers_1[n];
        int[] dp = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < size) {
            dp = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dp), java.util.stream.IntStream.of(0)).toArray()));
            i_2 = i_2 + 1;
        }
        int mask = 0;
        while (mask < size) {
            int row_1 = bitcount(mask);
            if (row_1 < n) {
                int col = 0;
                while (col < n) {
                    if (Math.floorMod((Math.floorDiv(mask, powers_1[col])), 2) == 0) {
                        int new_mask = mask + powers_1[col];
                        int value = dp[mask] + arr[row_1][col];
                        if (value > dp[new_mask]) {
dp[new_mask] = value;
                        }
                    }
                    col = col + 1;
                }
            }
            mask = mask + 1;
        }
        return dp[size - 1];
    }
    public static void main(String[] args) {
        MATRIX_2 = ((String[])(new String[]{"7 53 183 439 863 497 383 563 79 973 287 63 343 169 583", "627 343 773 959 943 767 473 103 699 303 957 703 583 639 913", "447 283 463 29 23 487 463 993 119 883 327 493 423 159 743", "217 623 3 399 853 407 103 983 89 463 290 516 212 462 350", "960 376 682 962 300 780 486 502 912 800 250 346 172 812 350", "870 456 192 162 593 473 915 45 989 873 823 965 425 329 803", "973 965 905 919 133 673 665 235 509 613 673 815 165 992 326", "322 148 972 962 286 255 941 541 265 323 925 281 601 95 973", "445 721 11 525 473 65 511 164 138 672 18 428 154 448 848", "414 456 310 312 798 104 566 520 302 248 694 976 430 392 198", "184 829 373 181 631 101 969 613 840 740 778 458 284 760 390", "821 461 843 513 17 901 711 993 293 157 274 94 192 156 574", "34 124 4 878 450 476 712 914 838 669 875 299 823 329 699", "815 559 813 459 522 788 168 586 966 232 308 833 251 631 107", "813 883 451 509 615 77 281 613 459 205 380 274 302 35 805"}));
        result = solution(((String[])(MATRIX_2)));
        System.out.println("solution() = " + _p(result));
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
