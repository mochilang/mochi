public class Main {

    static String compare_string(String string1, String string2) {
        String result = "";
        int count = 0;
        int i = 0;
        while (i < _runeLen(string1)) {
            String c1 = _substr(string1, i, i + 1);
            String c2 = _substr(string2, i, i + 1);
            if (!(c1.equals(c2))) {
                count = count + 1;
                result = result + "_";
            } else {
                result = result + c1;
            }
            i = i + 1;
        }
        if (count > 1) {
            return "";
        }
        return result;
    }

    static boolean contains_string(String[] arr, String value) {
        int i_1 = 0;
        while (i_1 < arr.length) {
            if ((arr[i_1].equals(value))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static String[] unique_strings(String[] arr) {
        String[] res = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < arr.length) {
            if (!(Boolean)contains_string(((String[])(res)), arr[i_2])) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(arr[i_2])).toArray(String[]::new)));
            }
            i_2 = i_2 + 1;
        }
        return res;
    }

    static String[] check(String[] binary) {
        String[] pi = ((String[])(new String[]{}));
        String[] current = ((String[])(binary));
        while (true) {
            String[] check1 = ((String[])(new String[]{}));
            int i_3 = 0;
            while (i_3 < current.length) {
                check1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(check1), java.util.stream.Stream.of("$")).toArray(String[]::new)));
                i_3 = i_3 + 1;
            }
            String[] temp = ((String[])(new String[]{}));
            i_3 = 0;
            while (i_3 < current.length) {
                int j = i_3 + 1;
                while (j < current.length) {
                    String k = String.valueOf(compare_string(current[i_3], current[j]));
                    if ((k.equals(""))) {
check1[i_3] = "*";
check1[j] = "*";
                        temp = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp), java.util.stream.Stream.of("X")).toArray(String[]::new)));
                    }
                    j = j + 1;
                }
                i_3 = i_3 + 1;
            }
            i_3 = 0;
            while (i_3 < current.length) {
                if ((check1[i_3].equals("$"))) {
                    pi = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(pi), java.util.stream.Stream.of(current[i_3])).toArray(String[]::new)));
                }
                i_3 = i_3 + 1;
            }
            if (temp.length == 0) {
                return pi;
            }
            current = ((String[])(unique_strings(((String[])(temp)))));
        }
    }

    static String[] decimal_to_binary(int no_of_variable, int[] minterms) {
        String[] temp_1 = ((String[])(new String[]{}));
        int idx = 0;
        while (idx < minterms.length) {
            int minterm = minterms[idx];
            String string = "";
            int i_4 = 0;
            while (i_4 < no_of_variable) {
                string = _p(Math.floorMod(minterm, 2)) + string;
                minterm = minterm / 2;
                i_4 = i_4 + 1;
            }
            temp_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp_1), java.util.stream.Stream.of(string)).toArray(String[]::new)));
            idx = idx + 1;
        }
        return temp_1;
    }

    static boolean is_for_table(String string1, String string2, int count) {
        int count_n = 0;
        int i_5 = 0;
        while (i_5 < _runeLen(string1)) {
            String c1_1 = _substr(string1, i_5, i_5 + 1);
            String c2_1 = _substr(string2, i_5, i_5 + 1);
            if (!(c1_1.equals(c2_1))) {
                count_n = count_n + 1;
            }
            i_5 = i_5 + 1;
        }
        return count_n == count;
    }

    static int count_ones(int[] row) {
        int c = 0;
        int j_1 = 0;
        while (j_1 < row.length) {
            if (row[j_1] == 1) {
                c = c + 1;
            }
            j_1 = j_1 + 1;
        }
        return c;
    }

    static String[] selection(int[][] chart, String[] prime_implicants) {
        String[] temp_2 = ((String[])(new String[]{}));
        int[] select = ((int[])(new int[]{}));
        int i_6 = 0;
        while (i_6 < chart.length) {
            select = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(select), java.util.stream.IntStream.of(0)).toArray()));
            i_6 = i_6 + 1;
        }
        int col = 0;
        while (col < chart[0].length) {
            int count_1 = 0;
            int row = 0;
            while (row < chart.length) {
                if (chart[row][col] == 1) {
                    count_1 = count_1 + 1;
                }
                row = row + 1;
            }
            if (count_1 == 1) {
                int rem = 0;
                row = 0;
                while (row < chart.length) {
                    if (chart[row][col] == 1) {
                        rem = row;
                    }
                    row = row + 1;
                }
select[rem] = 1;
            }
            col = col + 1;
        }
        i_6 = 0;
        while (i_6 < select.length) {
            if (select[i_6] == 1) {
                int j_2 = 0;
                while (j_2 < chart[0].length) {
                    if (chart[i_6][j_2] == 1) {
                        int r = 0;
                        while (r < chart.length) {
chart[r][j_2] = 0;
                            r = r + 1;
                        }
                    }
                    j_2 = j_2 + 1;
                }
                temp_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp_2), java.util.stream.Stream.of(prime_implicants[i_6])).toArray(String[]::new)));
            }
            i_6 = i_6 + 1;
        }
        while (true) {
            int[] counts = ((int[])(new int[]{}));
            int r_1 = 0;
            while (r_1 < chart.length) {
                counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(count_ones(((int[])(chart[r_1]))))).toArray()));
                r_1 = r_1 + 1;
            }
            int max_n = counts[0];
            int rem_1 = 0;
            int k_1 = 1;
            while (k_1 < counts.length) {
                if (counts[k_1] > max_n) {
                    max_n = counts[k_1];
                    rem_1 = k_1;
                }
                k_1 = k_1 + 1;
            }
            if (max_n == 0) {
                return temp_2;
            }
            temp_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp_2), java.util.stream.Stream.of(prime_implicants[rem_1])).toArray(String[]::new)));
            int j_3 = 0;
            while (j_3 < chart[0].length) {
                if (chart[rem_1][j_3] == 1) {
                    int r2 = 0;
                    while (r2 < chart.length) {
chart[r2][j_3] = 0;
                        r2 = r2 + 1;
                    }
                }
                j_3 = j_3 + 1;
            }
        }
    }

    static int count_char(String s, String ch) {
        int cnt = 0;
        int i_7 = 0;
        while (i_7 < _runeLen(s)) {
            if ((_substr(s, i_7, i_7 + 1).equals(ch))) {
                cnt = cnt + 1;
            }
            i_7 = i_7 + 1;
        }
        return cnt;
    }

    static int[][] prime_implicant_chart(String[] prime_implicants, String[] binary) {
        int[][] chart = ((int[][])(new int[][]{}));
        int i_8 = 0;
        while (i_8 < prime_implicants.length) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_4 = 0;
            while (j_4 < binary.length) {
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(0)).toArray()));
                j_4 = j_4 + 1;
            }
            chart = ((int[][])(appendObj(chart, row_1)));
            i_8 = i_8 + 1;
        }
        i_8 = 0;
        while (i_8 < prime_implicants.length) {
            int count_2 = count_char(prime_implicants[i_8], "_");
            int j_5 = 0;
            while (j_5 < binary.length) {
                if (((Boolean)(is_for_table(prime_implicants[i_8], binary[j_5], count_2)))) {
chart[i_8][j_5] = 1;
                }
                j_5 = j_5 + 1;
            }
            i_8 = i_8 + 1;
        }
        return chart;
    }

    static void main() {
        int no_of_variable = 3;
        int[] minterms = ((int[])(new int[]{1, 5, 7}));
        String[] binary = ((String[])(decimal_to_binary(no_of_variable, ((int[])(minterms)))));
        String[] prime_implicants = ((String[])(check(((String[])(binary)))));
        System.out.println("Prime Implicants are:");
        System.out.println(_p(prime_implicants));
        int[][] chart_1 = ((int[][])(prime_implicant_chart(((String[])(prime_implicants)), ((String[])(binary)))));
        String[] essential_prime_implicants = ((String[])(selection(((int[][])(chart_1)), ((String[])(prime_implicants)))));
        System.out.println("Essential Prime Implicants are:");
        System.out.println(_p(essential_prime_implicants));
    }
    public static void main(String[] args) {
        main();
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
