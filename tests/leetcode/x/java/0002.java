import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {
    static class FastScanner {
        private final byte[] data;
        private int idx = 0;
        FastScanner() throws IOException { data = System.in.readAllBytes(); }
        boolean hasNext() { while (idx < data.length && data[idx] <= ' ') idx++; return idx < data.length; }
        int nextInt() {
            while (idx < data.length && data[idx] <= ' ') idx++;
            int sign = 1;
            if (data[idx] == '-') { sign = -1; idx++; }
            int v = 0;
            while (idx < data.length && data[idx] > ' ') { v = v * 10 + (data[idx] - '0'); idx++; }
            return sign * v;
        }
    }

    static List<Integer> addLists(List<Integer> a, List<Integer> b) {
        List<Integer> out = new ArrayList<>();
        int i = 0, j = 0, carry = 0;
        while (i < a.size() || j < b.size() || carry > 0) {
            int sum = carry;
            if (i < a.size()) sum += a.get(i++);
            if (j < b.size()) sum += b.get(j++);
            out.add(sum % 10);
            carry = sum / 10;
        }
        return out;
    }

    static String format(List<Integer> arr) {
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < arr.size(); i++) {
            if (i > 0) sb.append(',');
            sb.append(arr.get(i));
        }
        return sb.append(']').toString();
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        if (!fs.hasNext()) return;
        int t = fs.nextInt();
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            List<Integer> a = new ArrayList<>();
            for (int i = 0; i < n; i++) a.add(fs.nextInt());
            int m = fs.nextInt();
            List<Integer> b = new ArrayList<>();
            for (int i = 0; i < m; i++) b.add(fs.nextInt());
            out.append(format(addLists(a, b)));
            if (tc + 1 < t) out.append('\n');
        }
        System.out.print(out);
    }
}
