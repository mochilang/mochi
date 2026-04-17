import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    private static String convert(String s, int numRows) {
        if (numRows <= 1 || numRows >= s.length()) return s;
        int cycle = 2 * numRows - 2;
        StringBuilder out = new StringBuilder();
        for (int row = 0; row < numRows; row++) {
            for (int i = row; i < s.length(); i += cycle) {
                out.append(s.charAt(i));
                int diag = i + cycle - 2 * row;
                if (row > 0 && row < numRows - 1 && diag < s.length()) out.append(s.charAt(diag));
            }
        }
        return out.toString();
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String first = br.readLine();
        if (first == null) return;
        int t = Integer.parseInt(first.trim());
        List<String> out = new ArrayList<>();
        for (int i = 0; i < t; i++) {
            String s = br.readLine();
            if (s == null) s = "";
            String rows = br.readLine();
            int numRows = rows == null ? 1 : Integer.parseInt(rows.trim());
            out.add(convert(s, numRows));
        }
        System.out.print(String.join("\n", out));
    }
}
