// bitmap-ppm-conversion-through-a-pipe.mochi
import java.util.*;

class Pixel {
    int R;
    int G;
    int B;
    Pixel(int R, int G, int B) {
        this.R = R;
        this.G = G;
        this.B = B;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Pixel other)) return false;
        return Objects.equals(this.R, other.R) && Objects.equals(this.G, other.G) && Objects.equals(this.B, other.B);
    }
    @Override public int hashCode() {
        return Objects.hash(R, G, B);
    }
}
class Bitmap {
    int cols;
    int rows;
    List<List<Pixel>> px;
    Bitmap(int cols, int rows, List<List<Pixel>> px) {
        this.cols = cols;
        this.rows = rows;
        this.px = px;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Bitmap other)) return false;
        return Objects.equals(this.cols, other.cols) && Objects.equals(this.rows, other.rows) && Objects.equals(this.px, other.px);
    }
    @Override public int hashCode() {
        return Objects.hash(cols, rows, px);
    }
}
public class BitmapPpmConversionThroughAPipe {
    static Pixel pixelFromRgb(int c) {
        int r = (Integer.parseInt((c / 65536))) % 256;
        int g = (Integer.parseInt((c / 256))) % 256;
        int b = c % 256;
        return new Pixel(r, g, b);
    }
    static int rgbFromPixel(Pixel p) {
        return p.R * 65536 + p.G * 256 + p.B;
    }
    static Bitmap NewBitmap(int x, int y) {
        List<List<Pixel>> data = Arrays.asList();
        int row = 0;
        while (row < y) {
            List<Pixel> r = Arrays.asList();
            int col = 0;
            while (col < x) {
                r.add(new Pixel(0, 0, 0));
                col = (int)(col + 1);
            }
            data.add(r);
            row = (int)(row + 1);
        }
        return new Bitmap(x, y, data);
    }
    static void FillRgb(Bitmap b, int c) {
        int y = 0;
        Pixel p = pixelFromRgb(c);
        while (y < b.rows) {
            int x = 0;
            while (x < b.cols) {
                List<List<Pixel>> px = b.px;
                List<List<Pixel>> row = px.get(y);
                row.set(x, p);
                px.set(y, row);
                b.px = px;
                x = (int)(x + 1);
            }
            y = (int)(y + 1);
        }
    }
    static boolean SetPxRgb(Bitmap b, int x, int y, int c) {
        if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
            return false;
        }
        List<List<Pixel>> px = b.px;
        List<List<Pixel>> row = px.get(y);
        row.set(x, pixelFromRgb(c));
        px.set(y, row);
        b.px = px;
        return true;
    }
    static int nextRand(int seed) {
        return (seed * 1664525 + 1013904223) % 2147483648;
    }
    static void main() {
        Bitmap bm = NewBitmap(400, 300);
        FillRgb(bm, 12615744);
        Object seed = now();
        int i = 0;
        while (i < 2000) {
            seed = nextRand(seed);
            int x = seed % 400;
            seed = nextRand(seed);
            int y = seed % 300;
            SetPxRgb(bm, x, y, 8405024);
            i = (int)(i + 1);
        }
        int x = 0;
        while (x < 400) {
            int y = 240;
            while (y < 245) {
                SetPxRgb(bm, x, y, 8405024);
                y = (int)(y + 1);
            }
            y = (int)(260);
            while (y < 265) {
                SetPxRgb(bm, x, y, 8405024);
                y = (int)(y + 1);
            }
            x = (int)(x + 1);
        }
        int y = 0;
        while (y < 300) {
            int x = 80;
            while (x < 85) {
                SetPxRgb(bm, x, y, 8405024);
                x = (int)(x + 1);
            }
            x = (int)(95);
            while (x < 100) {
                SetPxRgb(bm, x, y, 8405024);
                x = (int)(x + 1);
            }
            y = (int)(y + 1);
        }
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    main();
    }
}
