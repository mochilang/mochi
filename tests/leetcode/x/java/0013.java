import java.io.IOException;
import java.util.Map;

public class Main {
    static class FastScanner {
        private final byte[] data;
        private int idx = 0;
        FastScanner() throws IOException { data = System.in.readAllBytes(); }
        String next() {
            while (idx < data.length && data[idx] <= ' ') idx++;
            int start = idx;
            while (idx < data.length && data[idx] > ' ') idx++;
            return new String(data, start, idx - start);
        }
        boolean hasNext() {
            while (idx < data.length && data[idx] <= ' ') idx++;
            return idx < data.length;
        }
    }

    static final Map<Character, Integer> VALUES = Map.of(
        'I', 1, 'V', 5, 'X', 10, 'L', 50, 'C', 100, 'D', 500, 'M', 1000
    );

    static int romanToInt(String s) {
        int total = 0;
        for (int i = 0; i < s.length(); i++) {
            int cur = VALUES.get(s.charAt(i));
            int next = i + 1 < s.length() ? VALUES.get(s.charAt(i + 1)) : 0;
            total += cur < next ? -cur : cur;
        }
        return total;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        if (!fs.hasNext()) return;
        int t = Integer.parseInt(fs.next());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            out.append(romanToInt(fs.next()));
            if (i + 1 < t) out.append('\n');
        }
        System.out.print(out);
    }
}
