// csv-to-html-translation-1.mochi
import java.util.*;

public class CsvToHtmlTranslation1 {
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    int c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!";
    List<List<String>> rows = Arrays.asList();
    for (Number line : split(c, "\n")) {
        rows.add(split(line, ","));
    }
    System.out.println("<table>");
    for (List<String> row : rows) {
        String cells = "";
        for (String cell : row) {
            cells = cells + "<td>" + cell + "</td>";
        }
        System.out.println("    <tr>" + cells + "</tr>");
    }
    System.out.println("</table>");
    }
}
