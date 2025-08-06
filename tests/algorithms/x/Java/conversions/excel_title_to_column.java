public class Main {
    static String letters;

    static int excel_title_to_column(String title) {
        int result = 0;
        int i = 0;
        while (i < _runeLen(title)) {
            String ch = title.substring(i, i + 1);
            int value = 0;
            int idx = 0;
            boolean found = false;
            while (idx < _runeLen(letters)) {
                if ((letters.substring(idx, idx + 1).equals(ch))) {
                    value = idx + 1;
                    found = true;
                    break;
                }
                idx = idx + 1;
            }
            if (!found) {
                throw new RuntimeException(String.valueOf("title must contain only uppercase A-Z"));
            }
            result = result * 26 + value;
            i = i + 1;
        }
        return result;
    }

    static void main() {
        System.out.println(excel_title_to_column("A"));
        System.out.println(excel_title_to_column("B"));
        System.out.println(excel_title_to_column("AB"));
        System.out.println(excel_title_to_column("Z"));
    }
    public static void main(String[] args) {
        letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
