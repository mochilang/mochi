public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static String join_strings(String[] xs) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            res = res + xs[i];
            i = i + 1;
        }
        return res;
    }

    static String encrypt_message(int key, String message) {
        String result = "";
        int col = 0;
        while (col < key) {
            int pointer = col;
            while (pointer < _runeLen(message)) {
                result = result + _substr(message, pointer, pointer + 1);
                pointer = pointer + key;
            }
            col = col + 1;
        }
        return result;
    }

    static String decrypt_message(int key, String message) {
        int num_cols = (_runeLen(message) + key - 1) / key;
        int num_rows = key;
        int num_shaded_boxes = (num_cols * num_rows) - _runeLen(message);
        String[] plain_text = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < num_cols) {
            plain_text = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(plain_text), java.util.stream.Stream.of("")).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        int col_1 = 0;
        int row = 0;
        int index = 0;
        while (index < _runeLen(message)) {
plain_text[col_1] = plain_text[col_1] + _substr(message, index, index + 1);
            col_1 = col_1 + 1;
            if (col_1 == num_cols || (col_1 == num_cols - 1 && row >= num_rows - num_shaded_boxes)) {
                col_1 = 0;
                row = row + 1;
            }
            index = index + 1;
        }
        return join_strings(((String[])(plain_text)));
    }

    static void main() {
        System.out.println("Enter message: ");
        String message = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        int max_key = _runeLen(message) - 1;
        System.out.println("Enter key [2-" + _p(max_key) + "]: ");
        int key = Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""));
        System.out.println("Encryption/Decryption [e/d]: ");
        String mode = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        String text = "";
        String first = _substr(mode, 0, 1);
        if ((first.equals("e")) || (first.equals("E"))) {
            text = String.valueOf(encrypt_message(key, message));
        } else         if ((first.equals("d")) || (first.equals("D"))) {
            text = String.valueOf(decrypt_message(key, message));
        }
        System.out.println("Output:\n" + text + "|");
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
