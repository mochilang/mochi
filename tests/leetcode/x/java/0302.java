import java.io.*;
import java.util.*;

public class Main {
    static int minArea(String[] image, int x, int y) {
        int top = image.length, bottom = -1;
        int left = image[0].length(), right = -1;
        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[i].length(); j++) {
                if (image[i].charAt(j) == '1') {
                    if (i < top) top = i;
                    if (i > bottom) bottom = i;
                    if (j < left) left = j;
                    if (j > right) right = j;
                }
            }
        }
        return (bottom - top + 1) * (right - left + 1);
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = br.readLine()) != null) {
            line = line.trim();
            if (!line.isEmpty()) lines.add(line);
        }
        if (lines.isEmpty()) return;
        int t = Integer.parseInt(lines.get(0));
        int idx = 1;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            String[] parts = lines.get(idx++).split("\\s+");
            int r = Integer.parseInt(parts[0]);
            String[] image = new String[r];
            for (int i = 0; i < r; i++) image[i] = lines.get(idx++);
            parts = lines.get(idx++).split("\\s+");
            int x = Integer.parseInt(parts[0]);
            int y = Integer.parseInt(parts[1]);
            if (tc > 0) out.append("\n\n");
            out.append(minArea(image, x, y));
        }
        System.out.print(out);
    }
}
