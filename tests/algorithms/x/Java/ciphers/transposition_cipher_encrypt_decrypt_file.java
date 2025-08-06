public class Main {
    static int key;
    static String message;
    static String encrypted;
    static String decrypted;

    static String encrypt_message(int key, String message) {
        String result = "";
        int col = 0;
        while (col < key) {
            int pointer = col;
            while (pointer < _runeLen(message)) {
                result = result + message.substring(pointer, pointer+1);
                pointer = pointer + key;
            }
            col = col + 1;
        }
        return result;
    }

    static String decrypt_message(int key, String message) {
        int msg_len = _runeLen(message);
        int num_cols = msg_len / key;
        if (Math.floorMod(msg_len, key) != 0) {
            num_cols = num_cols + 1;
        }
        int num_rows = key;
        int num_shaded_boxes = num_cols * num_rows - msg_len;
        String[] plain = ((String[])(new String[]{}));
        int i = 0;
        while (i < num_cols) {
            plain = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(plain), java.util.stream.Stream.of("")).toArray(String[]::new)));
            i = i + 1;
        }
        int col_1 = 0;
        int row = 0;
        int idx = 0;
        while (idx < msg_len) {
            String ch = message.substring(idx, idx+1);
plain[col_1] = plain[col_1] + ch;
            col_1 = col_1 + 1;
            if (col_1 == num_cols || (col_1 == num_cols - 1 && row >= num_rows - num_shaded_boxes)) {
                col_1 = 0;
                row = row + 1;
            }
            idx = idx + 1;
        }
        String result_1 = "";
        i = 0;
        while (i < num_cols) {
            result_1 = result_1 + plain[i];
            i = i + 1;
        }
        return result_1;
    }
    public static void main(String[] args) {
        key = 6;
        message = "Harshil Darji";
        encrypted = String.valueOf(encrypt_message(key, message));
        System.out.println(encrypted);
        decrypted = String.valueOf(decrypt_message(key, encrypted));
        System.out.println(decrypted);
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
