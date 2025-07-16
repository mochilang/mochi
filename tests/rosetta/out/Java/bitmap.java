// bitmap.mochi
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
public class Bitmap {
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
    static Map<String,Integer> Extent(Bitmap b) {
        return new ColsRows(b.cols, b.rows);
    }
    static void Fill(Bitmap b, Pixel p) {
        int y = 0;
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
    static void FillRgb(Bitmap b, int c) {
        Fill(b, pixelFromRgb(c));
    }
    static boolean SetPx(Bitmap b, int x, int y, Pixel p) {
        if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
            return false;
        }
        List<List<Pixel>> px = b.px;
        List<List<Pixel>> row = px.get(y);
        row.set(x, p);
        px.set(y, row);
        b.px = px;
        return true;
    }
    static boolean SetPxRgb(Bitmap b, int x, int y, int c) {
        return SetPx(b, x, y, pixelFromRgb(c));
    }
    static Map<String,any> GetPx(Bitmap b, int x, int y) {
        if (x < 0 || x >= b.cols || y < 0 || y >= b.rows) {
            return new Ok(false);
        }
        List<List<Pixel>> row = b.px.get(y);
        return new OkPixel(true, row.get(x));
    }
    static Map<String,any> GetPxRgb(Bitmap b, int x, int y) {
        Map<String,any> r = GetPx(b, x, y);
        if (!((Map<String,any>)r).get("ok")) {
            return new Ok(false);
        }
        return new OkRgb(true, rgbFromPixel(((Map<String,any>)r).get("pixel")));
    }
    static int ppmSize(Bitmap b) {
        int header = "P6\n# Creator: Rosetta Code http://rosettacode.org/\n" + String.valueOf(b.cols) + " " + String.valueOf(b.rows) + "\n255\n";
        return header.size() + 3 * b.cols * b.rows;
    }
    static String pixelStr(Pixel p) {
        return "{" + String.valueOf(p.R) + " " + String.valueOf(p.G) + " " + String.valueOf(p.B) + "}";
    }
    static void main() {
        Bitmap bm = NewBitmap(300, 240);
        FillRgb(bm, 16711680);
        SetPxRgb(bm, 10, 20, 255);
        SetPxRgb(bm, 20, 30, 0);
        SetPxRgb(bm, 30, 40, 1056816);
        Map<String,any> c1 = GetPx(bm, 0, 0);
        Map<String,any> c2 = GetPx(bm, 10, 20);
        Map<String,any> c3 = GetPx(bm, 30, 40);
        System.out.println("Image size: " + String.valueOf(bm.cols) + " × " + String.valueOf(bm.rows));
        System.out.println(String.valueOf(ppmSize(bm)) + " bytes when encoded as PPM.");
        if (((Map<String,any>)c1).get("ok")) {
            System.out.println("Pixel at (0,0) is " + ((Number)pixelStr(((Map<String,any>)c1).get("pixel"))).doubleValue());
        }
        if (((Map<String,any>)c2).get("ok")) {
            System.out.println("Pixel at (10,20) is " + ((Number)pixelStr(((Map<String,any>)c2).get("pixel"))).doubleValue());
        }
        if (((Map<String,any>)c3).get("ok")) {
            any p = ((Map<String,any>)c3).get("pixel");
            int r16 = p.R * 257;
            int g16 = p.G * 257;
            int b16 = p.B * 257;
            System.out.println("Pixel at (30,40) has R=" + String.valueOf(r16) + ", G=" + String.valueOf(g16) + ", B=" + String.valueOf(b16));
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
