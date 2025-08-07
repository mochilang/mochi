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

    static boolean search(int target, int[] arr) {
        int left = 0;
        int right = arr.length - 1;
        while (left <= right) {
            int middle = Math.floorDiv((left + right), 2);
            if (arr[middle] == target) {
                return true;
            }
            if (arr[middle] < target) {
                left = middle + 1;
            } else {
                right = middle - 1;
            }
        }
        return false;
    }

    static int[] sort_int(int[] xs) {
        int[] arr = ((int[])(xs));
        int i_1 = 0;
        while (i_1 < arr.length) {
            int j = i_1 + 1;
            while (j < arr.length) {
                if (arr[j] < arr[i_1]) {
                    int tmp = arr[i_1];
arr[i_1] = arr[j];
arr[j] = tmp;
                }
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        return arr;
    }

    static int[] permutations_of_number(int n) {
        String s = _p(n);
        int[] d = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            d = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(d), java.util.stream.IntStream.of(Integer.parseInt(s.substring(i_2, i_2+1)))).toArray()));
            i_2 = i_2 + 1;
        }
        int[] res = ((int[])(new int[]{}));
        int a = 0;
        while (a < d.length) {
            int b = 0;
            while (b < d.length) {
                if (b != a) {
                    int c = 0;
                    while (c < d.length) {
                        if (c != a && c != b) {
                            int e = 0;
                            while (e < d.length) {
                                if (e != a && e != b && e != c) {
                                    int val = d[a] * 1000 + d[b] * 100 + d[c] * 10 + d[e];
                                    res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(val)).toArray()));
                                }
                                e = e + 1;
                            }
                        }
                        c = c + 1;
                    }
                }
                b = b + 1;
            }
            a = a + 1;
        }
        return res;
    }

    static int abs_int(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static boolean contains_int(int[] xs, int v) {
        int i_3 = 0;
        while (i_3 < xs.length) {
            if (xs[i_3] == v) {
                return true;
            }
            i_3 = i_3 + 1;
        }
        return false;
    }

    static int solution() {
        int[] prime_list = ((int[])(new int[]{}));
        int n = 1001;
        while (n < 10000) {
            if (((Boolean)(is_prime(n)))) {
                prime_list = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prime_list), java.util.stream.IntStream.of(n)).toArray()));
            }
            n = n + 2;
        }
        int[][] candidates = ((int[][])(new int[][]{}));
        int i_4 = 0;
        while (i_4 < prime_list.length) {
            int number = prime_list[i_4];
            int[] tmp_1 = ((int[])(new int[]{}));
            int[] perms = ((int[])(permutations_of_number(number)));
            int j_1 = 0;
            while (j_1 < perms.length) {
                int prime = perms[j_1];
                if (Math.floorMod(prime, 2) != 0 && ((Boolean)(search(prime, ((int[])(prime_list)))))) {
                    tmp_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(tmp_1), java.util.stream.IntStream.of(prime)).toArray()));
                }
                j_1 = j_1 + 1;
            }
            tmp_1 = ((int[])(sort_int(((int[])(tmp_1)))));
            if (tmp_1.length >= 3) {
                candidates = ((int[][])(appendObj(candidates, tmp_1)));
            }
            i_4 = i_4 + 1;
        }
        int[][] passed = ((int[][])(new int[][]{}));
        i_4 = 0;
        while (i_4 < candidates.length) {
            int[] candidate = ((int[])(candidates[i_4]));
            boolean found = false;
            int a_1 = 0;
            while (a_1 < candidate.length) {
                int b_1 = a_1 + 1;
                while (b_1 < candidate.length) {
                    int c_1 = b_1 + 1;
                    while (c_1 < candidate.length) {
                        int x = candidate[a_1];
                        int y = candidate[b_1];
                        int z = candidate[c_1];
                        if (abs_int(x - y) == abs_int(y - z) && x != y && x != z && y != z) {
                            int[] triple = ((int[])(sort_int(((int[])(new int[]{x, y, z})))));
                            passed = ((int[][])(appendObj(passed, triple)));
                            found = true;
                            break;
                        }
                        c_1 = c_1 + 1;
                    }
                    if (found) {
                        break;
                    }
                    b_1 = b_1 + 1;
                }
                if (found) {
                    break;
                }
                a_1 = a_1 + 1;
            }
            i_4 = i_4 + 1;
        }
        int[] answer_nums = ((int[])(new int[]{}));
        i_4 = 0;
        while (i_4 < passed.length) {
            int[] seq = ((int[])(passed[i_4]));
            int val_1 = Integer.parseInt(_p(_geti(seq, 0)) + _p(_geti(seq, 1)) + _p(_geti(seq, 2)));
            if (!(Boolean)contains_int(((int[])(answer_nums)), val_1)) {
                answer_nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(answer_nums), java.util.stream.IntStream.of(val_1)).toArray()));
            }
            i_4 = i_4 + 1;
        }
        int max_val = answer_nums[0];
        i_4 = 1;
        while (i_4 < answer_nums.length) {
            if (answer_nums[i_4] > max_val) {
                max_val = answer_nums[i_4];
            }
            i_4 = i_4 + 1;
        }
        return max_val;
    }
    public static void main(String[] args) {
        System.out.println(solution());
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
