import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.TreeSet;

public class Main {
    private static final class Point implements Comparable<Point> {
        final int x;
        final int y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public int compareTo(Point other) {
            if (x != other.x) return Integer.compare(x, other.x);
            return Integer.compare(y, other.y);
        }
    }

    private static int cross(Point o, Point a, Point b) {
        return (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
    }

    private static ArrayList<Point> solve(ArrayList<Point> pts) {
        Collections.sort(pts);
        ArrayList<Point> uniq = new ArrayList<>();
        for (Point p : pts) {
            if (uniq.isEmpty() || uniq.get(uniq.size() - 1).x != p.x || uniq.get(uniq.size() - 1).y != p.y) {
                uniq.add(p);
            }
        }
        if (uniq.size() <= 1) return uniq;

        ArrayList<Point> lower = new ArrayList<>();
        for (Point p : uniq) {
            while (lower.size() >= 2 && cross(lower.get(lower.size() - 2), lower.get(lower.size() - 1), p) < 0) {
                lower.remove(lower.size() - 1);
            }
            lower.add(p);
        }

        ArrayList<Point> upper = new ArrayList<>();
        for (int i = uniq.size() - 1; i >= 0; i--) {
            Point p = uniq.get(i);
            while (upper.size() >= 2 && cross(upper.get(upper.size() - 2), upper.get(upper.size() - 1), p) < 0) {
                upper.remove(upper.size() - 1);
            }
            upper.add(p);
        }

        TreeSet<Point> set = new TreeSet<>();
        set.addAll(lower);
        set.addAll(upper);
        return new ArrayList<>(set);
    }

    public static void main(String[] args) throws Exception {
        FastScanner fs = new FastScanner();
        int t = fs.nextInt();
        if (t == Integer.MIN_VALUE) return;
        StringBuilder out = new StringBuilder();
        for (int tc = 0; tc < t; tc++) {
            int n = fs.nextInt();
            ArrayList<Point> pts = new ArrayList<>();
            for (int i = 0; i < n; i++) {
                pts.add(new Point(fs.nextInt(), fs.nextInt()));
            }
            ArrayList<Point> hull = solve(pts);
            if (tc > 0) out.append('\n');
            out.append(hull.size()).append('\n');
            for (Point p : hull) out.append(p.x).append(' ').append(p.y).append('\n');
        }
        System.out.print(out);
    }

    private static final class FastScanner {
        private final byte[] buffer = new byte[1 << 16];
        private int ptr = 0, len = 0;

        private int read() throws IOException {
            if (ptr >= len) {
                len = System.in.read(buffer);
                ptr = 0;
                if (len <= 0) return -1;
            }
            return buffer[ptr++];
        }

        int nextInt() throws IOException {
            int c;
            do {
                c = read();
                if (c == -1) return Integer.MIN_VALUE;
            } while (c <= ' ');
            int sign = 1;
            if (c == '-') {
                sign = -1;
                c = read();
            }
            int val = 0;
            while (c > ' ') {
                val = val * 10 + (c - '0');
                c = read();
            }
            return val * sign;
        }
    }
}
