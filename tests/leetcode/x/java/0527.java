import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Main {
    private static final class Item {
        String word;
        int idx;

        Item(String word, int idx) {
            this.word = word;
            this.idx = idx;
        }
    }

    private static int lcp(String a, String b) {
        int i = 0;
        while (i < a.length() && i < b.length() && a.charAt(i) == b.charAt(i)) {
            i++;
        }
        return i;
    }

    private static String abbreviate(String word, int prefix) {
        if (word.length() - prefix <= 2) {
            return word;
        }
        return word.substring(0, prefix) + (word.length() - prefix - 1) + word.charAt(word.length() - 1);
    }

    private static List<String> solve(List<String> words) {
        Map<String, ArrayList<Item>> groups = new HashMap<>();
        for (int i = 0; i < words.size(); i++) {
            String word = words.get(i);
            String key = word.length() + "|" + word.charAt(0) + "|" + word.charAt(word.length() - 1);
            groups.computeIfAbsent(key, k -> new ArrayList<>()).add(new Item(word, i));
        }
        ArrayList<String> ans = new ArrayList<>(Collections.nCopies(words.size(), ""));
        for (ArrayList<Item> group : groups.values()) {
            group.sort((a, b) -> a.word.compareTo(b.word));
            for (int j = 0; j < group.size(); j++) {
                int need = 1;
                if (j > 0) {
                    need = Math.max(need, lcp(group.get(j).word, group.get(j - 1).word) + 1);
                }
                if (j + 1 < group.size()) {
                    need = Math.max(need, lcp(group.get(j).word, group.get(j + 1).word) + 1);
                }
                ans.set(group.get(j).idx, abbreviate(group.get(j).word, need));
            }
        }
        return ans;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) {
            return;
        }
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            ArrayList<String> words = new ArrayList<>();
            for (int i = 0; i < n; i++) {
                words.add(fs.next());
            }
            List<String> ans = solve(words);
            if (tc > 0) {
                out.append('\n');
            }
            out.append(ans.size()).append('\n');
            for (String s : ans) {
                out.append(s).append('\n');
            }
        }
        System.out.print(out);
    }

    private static final class FastScanner {
        private final byte[] buffer = new byte[1 << 16];
        private int ptr = 0;
        private int len = 0;

        private int read() throws IOException {
            if (ptr >= len) {
                len = System.in.read(buffer);
                ptr = 0;
                if (len <= 0) {
                    return -1;
                }
            }
            return buffer[ptr++];
        }

        int nextInt() throws IOException {
            String s = next();
            if (s == null) {
                return Integer.MIN_VALUE;
            }
            return Integer.parseInt(s);
        }

        String next() throws IOException {
            int c;
            do {
                c = read();
                if (c == -1) {
                    return null;
                }
            } while (c <= ' ');
            StringBuilder sb = new StringBuilder();
            while (c > ' ') {
                sb.append((char) c);
                c = read();
            }
            return sb.toString();
        }
    }
}
