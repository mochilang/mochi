public class Main {
    static String c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!";
    static String[][] rows = new String[][]{};
    static boolean headings = true;

    public static void main(String[] args) {
        for (var line : c.split("\n")) {
            rows = appendObj(rows, line.split(","));
        }
        System.out.println("<table>");
        if (headings) {
            if (rows.length > 0) {
                String th = "";
                for (String h : rows[0]) {
                    th = th + "<th>" + h + "</th>";
                }
                System.out.println("   <thead>");
                System.out.println("      <tr>" + th + "</tr>");
                System.out.println("   </thead>");
                System.out.println("   <tbody>");
                int i = 1;
                while (i < rows.length) {
                    String cells = "";
                    for (String cell : rows[i]) {
                        cells = cells + "<td>" + cell + "</td>";
                    }
                    System.out.println("      <tr>" + cells + "</tr>");
                    i = i + 1;
                }
                System.out.println("   </tbody>");
            }
        } else {
            for (String[] row : rows) {
                String cells = "";
                for (String cell : row) {
                    cells = cells + "<td>" + cell + "</td>";
                }
                System.out.println("    <tr>" + cells + "</tr>");
            }
        }
        System.out.println("</table>");
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
