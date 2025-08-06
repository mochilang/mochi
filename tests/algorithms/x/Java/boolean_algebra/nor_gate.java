public class Main {

    static int nor_gate(int input_1, int input_2) {
        if (input_1 == 0 && input_2 == 0) {
            return 1;
        }
        return 0;
    }

    static String center(String s, int width) {
        int total = width - _runeLen(s);
        if (total <= 0) {
            return s;
        }
        int left = total / 2;
        int right = total - left;
        String res = s;
        int i = 0;
        while (i < left) {
            res = " " + res;
            i = i + 1;
        }
        int j = 0;
        while (j < right) {
            res = res + " ";
            j = j + 1;
        }
        return res;
    }

    static String make_table_row(int i, int j) {
        int output = nor_gate(i, j);
        return "| " + String.valueOf(center(_p(i), 8)) + " | " + String.valueOf(center(_p(j), 8)) + " | " + String.valueOf(center(_p(output), 8)) + " |";
    }

    static String truth_table() {
        return "Truth Table of NOR Gate:\n" + "| Input 1 | Input 2 | Output  |\n" + String.valueOf(make_table_row(0, 0)) + "\n" + String.valueOf(make_table_row(0, 1)) + "\n" + String.valueOf(make_table_row(1, 0)) + "\n" + String.valueOf(make_table_row(1, 1));
    }
    public static void main(String[] args) {
        System.out.println(nor_gate(0, 0));
        System.out.println(nor_gate(0, 1));
        System.out.println(nor_gate(1, 0));
        System.out.println(nor_gate(1, 1));
        System.out.println(truth_table());
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
}
