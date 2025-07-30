public class Main {
    static class Pt {
        double x;
        double y;
        boolean inf;
        Pt(double x, double y, boolean inf) {
            this.x = x;
            this.y = y;
            this.inf = inf;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'inf': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(inf));
        }
    }

    static double bCoeff;

    static Pt zero() {
        return new Pt(0.0, 0.0, true);
    }

    static boolean isZero(Pt p) {
        return p.inf;
    }

    static Pt neg(Pt p) {
        return new Pt(p.x, -p.y, p.inf);
    }

    static Pt dbl(Pt p) {
        if (isZero(p)) {
            return p;
        }
        double L = (3.0 * p.x * p.x) / (2.0 * p.y);
        double x = L * L - 2.0 * p.x;
        return new Pt(x, L * (p.x - x) - p.y, false);
    }

    static Pt add(Pt p, Pt q) {
        if (isZero(p)) {
            return q;
        }
        if (isZero(q)) {
            return p;
        }
        if (p.x == q.x) {
            if (p.y == q.y) {
                return dbl(p);
            }
            return zero();
        }
        double L = (q.y - p.y) / (q.x - p.x);
        double x = L * L - p.x - q.x;
        return new Pt(x, L * (p.x - x) - p.y, false);
    }

    static Pt mul(Pt p, int n) {
        Pt r = zero();
        Pt q = p;
        int k = n;
        while (k > 0) {
            if (Math.floorMod(k, 2) == 1) {
                r = add(r, q);
            }
            q = dbl(q);
            k = k / 2;
        }
        return r;
    }

    static double cbrtApprox(double x) {
        double guess = x;
        int i = 0;
        while (i < 40) {
            guess = (2.0 * guess + x / (guess * guess)) / 3.0;
            i = i + 1;
        }
        return guess;
    }

    static Pt fromY(double y) {
        return new Pt(cbrtApprox(y * y - bCoeff), y, false);
    }

    static void show(String s, Pt p) {
        if (isZero(p)) {
            System.out.println(s + "Zero");
        } else {
            System.out.println(s + "(" + String.valueOf(p.x) + ", " + String.valueOf(p.y) + ")");
        }
    }

    static void main() {
        Pt a = fromY(1.0);
        Pt b = fromY(2.0);
        show("a = ", a);
        show("b = ", b);
        Pt c = add(a, b);
        show("c = a + b = ", c);
        Pt d = neg(c);
        show("d = -c = ", d);
        show("c + d = ", add(c, d));
        show("a + b + d = ", add(a, add(b, d)));
        show("a * 12345 = ", mul(a, 12345));
    }
    public static void main(String[] args) {
        bCoeff = 7.0;
        main();
    }
}
