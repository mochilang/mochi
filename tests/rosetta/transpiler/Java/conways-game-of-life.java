public class Main {
    static class Field {
        boolean[][] s;
        int w;
        int h;
        Field(boolean[][] s, int w, int h) {
            this.s = s;
            this.w = w;
            this.h = h;
        }
        @Override public String toString() {
            return String.format("{'s': %s, 'w': %s, 'h': %s}", String.valueOf(s), String.valueOf(w), String.valueOf(h));
        }
    }

    static class Life {
        Field a;
        Field b;
        int w;
        int h;
        Life(Field a, Field b, int w, int h) {
            this.a = a;
            this.b = b;
            this.w = w;
            this.h = h;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'b': %s, 'w': %s, 'h': %s}", String.valueOf(a), String.valueOf(b), String.valueOf(w), String.valueOf(h));
        }
    }

    static int seed = 1;

    static int randN(int n) {
        seed = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
        return Math.floorMod(seed, n);
    }

    static Field newField(int w, int h) {
        boolean[][] rows = new boolean[][]{};
        int y = 0;
        while (y < h) {
            boolean[] row = new boolean[]{};
            int x = 0;
            while (x < w) {
                row = appendBool(row, false);
                x = x + 1;
            }
            rows = appendObj(rows, row);
            y = y + 1;
        }
        return new Field(rows, w, h);
    }

    static void setCell(Field f, int x, int y, boolean b) {
        boolean[][] rows = f.s;
        boolean[] row = rows[y];
row[x] = b;
rows[y] = row;
f.s = rows;
    }

    static boolean state(Field f, int x, int y) {
        while (y < 0) {
            y = y + f.h;
        }
        while (x < 0) {
            x = x + f.w;
        }
        return f.s[Math.floorMod(y, f.h)][Math.floorMod(x, f.w)];
    }

    static boolean nextState(Field f, int x, int y) {
        int count = 0;
        int dy = -1;
        while (dy <= 1) {
            int dx = -1;
            while (dx <= 1) {
                if (!(dx == 0 && dy == 0) && state(f, x + dx, y + dy)) {
                    count = count + 1;
                }
                dx = dx + 1;
            }
            dy = dy + 1;
        }
        return count == 3 || (count == 2 && state(f, x, y));
    }

    static Life newLife(int w, int h) {
        Field a = newField(w, h);
        int i = 0;
        while (i < (w * h / 2)) {
            setCell(a, randN(w), randN(h), true);
            i = i + 1;
        }
        return new Life(a, newField(w, h), w, h);
    }

    static void step(Life l) {
        int y = 0;
        while (y < l.h) {
            int x = 0;
            while (x < l.w) {
                setCell(l.b, x, y, nextState(l.a, x, y));
                x = x + 1;
            }
            y = y + 1;
        }
        Field tmp = l.a;
l.a = l.b;
l.b = tmp;
    }

    static String lifeString(Life l) {
        String out = "";
        int y = 0;
        while (y < l.h) {
            int x = 0;
            while (x < l.w) {
                if (state(l.a, x, y)) {
                    out = out + "*";
                } else {
                    out = out + " ";
                }
                x = x + 1;
            }
            out = out + "\n";
            y = y + 1;
        }
        return out;
    }

    static void main() {
        Life l = newLife(80, 15);
        int i = 0;
        while (i < 300) {
            step(l);
            System.out.println("\f");
            System.out.println(lifeString(l));
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
