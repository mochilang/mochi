public class Main {
    static java.util.Map<String,Object> r1 = getCombs(1, 7, true);
    static java.util.Map<String,Object> r2 = getCombs(3, 9, true);
    static java.util.Map<String,Object> r3 = getCombs(0, 9, false);

    static boolean validComb(int a, int b, int c, int d, int e, int f, int g) {
        int square1 = a + b;
        int square2 = b + c + d;
        int square3 = d + e + f;
        int square4 = f + g;
        return square1 == square2 && square2 == square3 && square3 == square4;
    }

    static boolean isUnique(int a, int b, int c, int d, int e, int f, int g) {
        Object[] nums = new Object[]{a, b, c, d, e, f, g};
        int i = 0;
        while (i < nums.length) {
            int j = i + 1;
            while (j < nums.length) {
                if (nums[i] == nums[j]) {
                    return false;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return true;
    }

    static java.util.Map<String,Object> getCombs(int low, int high, boolean unique) {
        int[][] valid = new int[][]{};
        int count = 0;
        for (int b = low; b < (high + 1); b++) {
            for (int c = low; c < (high + 1); c++) {
                for (int d = low; d < (high + 1); d++) {
                    int s = b + c + d;
                    for (int e = low; e < (high + 1); e++) {
                        for (int f = low; f < (high + 1); f++) {
                            int a = s - b;
                            int g = s - f;
                            if (a < low || a > high) {
                                continue;
                            }
                            if (g < low || g > high) {
                                continue;
                            }
                            if (d + e + f != s) {
                                continue;
                            }
                            if (f + g != s) {
                                continue;
                            }
                            if (!(Boolean)unique || isUnique(a, b, c, d, e, f, g)) {
                                valid = java.util.stream.Stream.concat(java.util.Arrays.stream(valid), java.util.stream.Stream.of(new int[]{a, b, c, d, e, f, g})).toArray(int[][]::new);
                                count = count + 1;
                            }
                        }
                    }
                }
            }
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("count", count, "list", valid));
    }
    public static void main(String[] args) {
        System.out.println(String.valueOf((int)(r1.get("count"))) + " unique solutions in 1 to 7");
        System.out.println(java.util.Arrays.deepToString((int[][])(r1.get("list"))));
        System.out.println(String.valueOf((int)(r2.get("count"))) + " unique solutions in 3 to 9");
        System.out.println(java.util.Arrays.deepToString((int[][])(r2.get("list"))));
        System.out.println(String.valueOf((int)(r3.get("count"))) + " non-unique solutions in 0 to 9");
    }
}
