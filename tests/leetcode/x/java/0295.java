import java.io.*;
import java.util.*;

public class Main {
    static class MedianFinder {
        private final ArrayList<Integer> data = new ArrayList<>();

        void addNum(int num) {
            int pos = Collections.binarySearch(data, num);
            if (pos < 0) pos = -pos - 1;
            data.add(pos, num);
        }

        double findMedian() {
            int n = data.size();
            if ((n & 1) == 1) return data.get(n / 2);
            return (data.get(n / 2 - 1) + data.get(n / 2)) / 2.0;
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        ArrayList<String> lines = new ArrayList<>();
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
            int m = Integer.parseInt(lines.get(idx++));
            MedianFinder mf = new MedianFinder();
            if (tc > 0) out.append("\n\n");
            boolean first = true;
            for (int i = 0; i < m; i++) {
                String[] parts = lines.get(idx++).split("\\s+");
                if (parts[0].equals("addNum")) {
                    mf.addNum(Integer.parseInt(parts[1]));
                } else {
                    if (!first) out.append('\n');
                    first = false;
                    out.append(String.format(Locale.US, "%.1f", mf.findMedian()));
                }
            }
        }
        System.out.print(out);
    }
}
