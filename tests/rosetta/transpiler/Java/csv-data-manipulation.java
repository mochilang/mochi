public class Main {
    static String[][] rows = new String[][]{new String[]{"A", "B", "C"}, new String[]{"1", "2", "3"}, new String[]{"4", "5", "6"}, new String[]{"7", "8", "9"}};
    static int i = 1;

    static String join(String[] xs, String sep) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            if (i > 0) {
                res = res + sep;
            }
            res = res + xs[i];
            i = i + 1;
        }
        return res;
    }

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (str.length() > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)));
        while (i < str.length()) {
            n = n * 10 + (int)(((int)digits.getOrDefault(str.substring(i, i + 1), 0)));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }
    public static void main(String[] args) {
rows[0] = java.util.stream.Stream.concat(java.util.Arrays.stream(rows[0]), java.util.stream.Stream.of("SUM")).toArray(String[]::new);
        while (i < rows.length) {
            int sum = 0;
            for (String s : rows[i]) {
                sum = sum + Integer.parseInt(s);
            }
rows[i] = java.util.stream.Stream.concat(java.util.Arrays.stream(rows[i]), java.util.stream.Stream.of(String.valueOf(sum))).toArray(String[]::new);
            i = i + 1;
        }
        for (String[] r : rows) {
            System.out.println(join(r, ","));
        }
    }
}
