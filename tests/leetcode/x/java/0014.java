import java.io.IOException;

public class Main {
    static class FastScanner {
        private final byte[] data; private int idx = 0;
        FastScanner() throws IOException { data = System.in.readAllBytes(); }
        String next() {
            while (idx < data.length && data[idx] <= ' ') idx++;
            int start = idx;
            while (idx < data.length && data[idx] > ' ') idx++;
            return new String(data, start, idx - start);
        }
        boolean hasNext() { while (idx < data.length && data[idx] <= ' ') idx++; return idx < data.length; }
    }
    static String lcp(String[] strs) {
        String prefix = strs[0];
        while (true) {
            boolean ok = true;
            for (String s : strs) if (!s.startsWith(prefix)) { ok = false; break; }
            if (ok) return prefix;
            prefix = prefix.substring(0, prefix.length() - 1);
        }
    }
    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        if (!fs.hasNext()) return;
        int t = Integer.parseInt(fs.next());
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = Integer.parseInt(fs.next());
            String[] strs = new String[n];
            for (int i = 0; i < n; i++) strs[i] = fs.next();
            out.append('"').append(lcp(strs)).append('"');
            if (tc + 1 < t) out.append('\n');
        }
        System.out.print(out);
    }
}
