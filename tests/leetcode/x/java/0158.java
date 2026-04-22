import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class Main {
    static String quote(String s) {
        return "\"" + s + "\"";
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) lines.add(line);
        if (lines.isEmpty()) return;
        int tc = Integer.parseInt(lines.get(0));
        int idx = 1;
        List<String> out = new ArrayList<>();
        for (int t = 0; t < tc; t++) {
            String data = lines.get(idx++);
            int q = Integer.parseInt(lines.get(idx++));
            int pos = 0;
            List<String> block = new ArrayList<>();
            block.add(Integer.toString(q));
            for (int i = 0; i < q; i++) {
                int n = Integer.parseInt(lines.get(idx++));
                int end = Math.min(pos + n, data.length());
                String part = data.substring(pos, end);
                pos = end;
                block.add(quote(part));
            }
            out.add(String.join("\n", block));
        }
        System.out.print(String.join("\n\n", out));
    }
}
