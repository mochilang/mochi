public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static String[] parse_names(String line) {
        String[] names = ((String[])(new String[]{}));
        String current = "";
        int i = 0;
        while (i < _runeLen(line)) {
            String ch = _substr(line, i, i + 1);
            if ((ch.equals(","))) {
                names = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(names), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else             if (!(ch.equals("\""))) {
                current = current + ch;
            }
            i = i + 1;
        }
        names = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(names), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return names;
    }

    static String[] insertion_sort(String[] arr) {
        String[] a = ((String[])(arr));
        int i_1 = 1;
        while (i_1 < a.length) {
            String key = a[i_1];
            int j = i_1 - 1;
            while (j >= 0 && (a[j].compareTo(key) > 0)) {
a[j + 1] = a[j];
                j = j - 1;
            }
a[j + 1] = key;
            i_1 = i_1 + 1;
        }
        return a;
    }

    static int letter_value(String ch) {
        String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int idx = 0;
        while (idx < _runeLen(alphabet)) {
            if ((_substr(alphabet, idx, idx + 1).equals(ch))) {
                return idx + 1;
            }
            idx = idx + 1;
        }
        return 0;
    }

    static int name_score(String name) {
        int score = 0;
        int i_2 = 0;
        while (i_2 < _runeLen(name)) {
            score = score + letter_value(_substr(name, i_2, i_2 + 1));
            i_2 = i_2 + 1;
        }
        return score;
    }

    static void main() {
        String line = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        String[] names_1 = ((String[])(insertion_sort(((String[])(parse_names(line))))));
        int total = 0;
        int i_3 = 0;
        while (i_3 < names_1.length) {
            total = total + (i_3 + 1) * name_score(names_1[i_3]);
            i_3 = i_3 + 1;
        }
        System.out.println(_p(total));
    }
    public static void main(String[] args) {
        main();
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
