public class Main {

    static int digitSumMod(int n, int base) {
        int sum = 0;
        int j = n;
        while (j > 0) {
            sum = sum + Math.floorMod(j, base);
            j = j / base;
        }
        return Math.floorMod(sum, base);
    }

    static int[] fairshareList(int n, int base) {
        int[] res = new int[]{};
        int i = 0;
        while (i < n) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(digitSumMod(i, base))).toArray();
            i = i + 1;
        }
        return res;
    }

    static int[] sortInts(int[] xs) {
        int[] arr = xs;
        int i_1 = 0;
        while (i_1 < arr.length) {
            int j_1 = 0;
            while (j_1 < arr.length - 1) {
                if (arr[j_1] > arr[j_1 + 1]) {
                    int t = arr[j_1];
arr[j_1] = arr[j_1 + 1];
arr[j_1 + 1] = t;
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return arr;
    }

    static String turns(int n, int base) {
        int[] counts = new int[]{};
        int i_2 = 0;
        while (i_2 < base) {
            counts = java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(0)).toArray();
            i_2 = i_2 + 1;
        }
        i_2 = 0;
        while (i_2 < n) {
            int v = digitSumMod(i_2, base);
counts[v] = counts[v] + 1;
            i_2 = i_2 + 1;
        }
        java.util.Map<Integer,Integer> freq = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        int[] fkeys = new int[]{};
        i_2 = 0;
        while (i_2 < base) {
            int c = counts[i_2];
            if (c > 0) {
                if (((Boolean)(freq.containsKey(c)))) {
freq.put(c, (int)(((int)(freq).getOrDefault(c, 0))) + 1);
                } else {
freq.put(c, 1);
                    fkeys = java.util.stream.IntStream.concat(java.util.Arrays.stream(fkeys), java.util.stream.IntStream.of(c)).toArray();
                }
            }
            i_2 = i_2 + 1;
        }
        int total = 0;
        i_2 = 0;
        while (i_2 < fkeys.length) {
            total = total + (int)(((int)(freq).getOrDefault(fkeys[i_2], 0)));
            i_2 = i_2 + 1;
        }
        if (total != base) {
            return "only " + _p(total) + " have a turn";
        }
        fkeys = sortInts(fkeys);
        String res_1 = "";
        i_2 = 0;
        while (i_2 < fkeys.length) {
            if (i_2 > 0) {
                res_1 = res_1 + " or ";
            }
            res_1 = res_1 + _p(_geti(fkeys, i_2));
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static void main() {
        int[] bases1 = new int[]{2, 3, 5, 11};
        int i_3 = 0;
        while (i_3 < bases1.length) {
            int b = bases1[i_3];
            System.out.println((String)(_padStart(_p(b), 2, " ")) + " : " + _p(fairshareList(25, b)));
            i_3 = i_3 + 1;
        }
        System.out.println("");
        System.out.println("How many times does each get a turn in 50000 iterations?");
        int[] bases2 = new int[]{191, 1377, 49999, 50000, 50001};
        i_3 = 0;
        while (i_3 < bases2.length) {
            int b_1 = bases2[i_3];
            String t_1 = String.valueOf(turns(50000, b_1));
            System.out.println("  With " + _p(b_1) + " people: " + t_1);
            i_3 = i_3 + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static String _padStart(String s, int width, String pad) {
        String out = s;
        while (out.length() < width) { out = pad + out; }
        return out;
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
