public class Main {
    static String c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!";
    static String[][] rows = new String[][]{};

    static String[] split(String s, String sep) {
        String[] out = new String[]{};
        int start = 0;
        int i = 0;
        int n = sep.length();
        while (i <= s.length() - n) {
            if ((s.substring(i, i + n).equals(sep))) {
                out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(s.substring(start, i))).toArray(String[]::new);
                i = i + n;
                start = i;
            } else {
                i = i + 1;
            }
        }
        out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(s.substring(start, s.length()))).toArray(String[]::new);
        return out;
    }

    static String htmlEscape(String s) {
        String out = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals("&"))) {
                out = out + "&amp;";
            } else             if ((ch.equals("<"))) {
                out = out + "&lt;";
            } else             if ((ch.equals(">"))) {
                out = out + "&gt;";
            } else {
                out = out + ch;
            }
            i = i + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        for (var line : c.split("\n")) {
            rows = appendObj(rows, line.split(","));
        }
        System.out.println("<table>");
        for (String[] row : rows) {
            String cells = "";
            for (String cell : row) {
                cells = cells + "<td>" + String.valueOf(htmlEscape(cell)) + "</td>";
            }
            System.out.println("    <tr>" + cells + "</tr>");
        }
        System.out.println("</table>");
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
