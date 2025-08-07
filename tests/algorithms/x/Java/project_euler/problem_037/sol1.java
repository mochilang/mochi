public class Main {

    static boolean is_prime(int number) {
        if (1 < number && number < 4) {
            return true;
        }
        if (number < 2 || Math.floorMod(number, 2) == 0 || Math.floorMod(number, 3) == 0) {
            return false;
        }
        int i = 5;
        while (i * i <= number) {
            if (Math.floorMod(number, i) == 0 || Math.floorMod(number, (i + 2)) == 0) {
                return false;
            }
            i = i + 6;
        }
        return true;
    }

    static int[] list_truncated_nums(int n) {
        String str_num = _p(n);
        int[] list_nums = ((int[])(new int[]{n}));
        int i_1 = 1;
        int length = _runeLen(str_num);
        while (i_1 < length) {
            int right = Integer.parseInt(_substr(str_num, i_1, length));
            int left = Integer.parseInt(_substr(str_num, 0, length - i_1));
            list_nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(list_nums), java.util.stream.IntStream.of(right)).toArray()));
            list_nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(list_nums), java.util.stream.IntStream.of(left)).toArray()));
            i_1 = i_1 + 1;
        }
        return list_nums;
    }

    static boolean validate(int n) {
        String s = _p(n);
        int length_1 = _runeLen(s);
        if (length_1 > 3) {
            int last3 = Integer.parseInt(_substr(s, length_1 - 3, length_1));
            int first3 = Integer.parseInt(_substr(s, 0, 3));
            if (!(((Boolean)(is_prime(last3))) && ((Boolean)(is_prime(first3))))) {
                return false;
            }
        }
        return true;
    }

    static int[] compute_truncated_primes(int count) {
        int[] list_truncated_primes = ((int[])(new int[]{}));
        int num = 13;
        while (list_truncated_primes.length != count) {
            if (((Boolean)(validate(num)))) {
                int[] list_nums_1 = ((int[])(list_truncated_nums(num)));
                boolean all_prime = true;
                int j = 0;
                while (j < list_nums_1.length) {
                    if (!(Boolean)is_prime(list_nums_1[j])) {
                        all_prime = false;
                        break;
                    }
                    j = j + 1;
                }
                if (all_prime) {
                    list_truncated_primes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(list_truncated_primes), java.util.stream.IntStream.of(num)).toArray()));
                }
            }
            num = num + 2;
        }
        return list_truncated_primes;
    }

    static int solution() {
        int[] primes = ((int[])(compute_truncated_primes(11)));
        int total = 0;
        int i_2 = 0;
        while (i_2 < primes.length) {
            total = total + primes[i_2];
            i_2 = i_2 + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        System.out.println("sum(compute_truncated_primes(11)) = " + _p(solution()));
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
