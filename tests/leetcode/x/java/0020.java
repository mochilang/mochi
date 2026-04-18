import java.io.IOException;

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

    static boolean isValid(String s) {
        char[] stack = new char[s.length()];
        int top = 0;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (ch == '(' || ch == '[' || ch == '{') {
                stack[top++] = ch;
            } else {
                if (top == 0) return false;
                char open = stack[--top];
                if ((ch == ')' && open != '(') ||
                    (ch == ']' && open != '[') ||
                    (ch == '}' && open != '{')) return false;
            }
        }
        return top == 0;
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        if (!fs.hasNext()) return;
        int t = Integer.parseInt(fs.next());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            out.append(isValid(fs.next()) ? "true" : "false");
            if (i + 1 < t) out.append('\n');
        }
        System.out.print(out);
    }
}
