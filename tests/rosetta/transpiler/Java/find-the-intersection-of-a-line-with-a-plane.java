public class Main {
    static class Vector {
        double x;
        double y;
        double z;
        Vector(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z));
        }
    }


    static Vector add(Vector a, Vector b) {
        return new Vector(a.x + b.x, a.y + b.y, a.z + b.z);
    }

    static Vector sub(Vector a, Vector b) {
        return new Vector(a.x - b.x, a.y - b.y, a.z - b.z);
    }

    static Vector mul(Vector v, double s) {
        return new Vector(v.x * s, v.y * s, v.z * s);
    }

    static double dot(Vector a, Vector b) {
        return a.x * b.x + a.y * b.y + a.z * b.z;
    }

    static Vector intersectPoint(Vector rv, Vector rp, Vector pn, Vector pp) {
        Vector diff = sub(rp, pp);
        double prod1 = dot(diff, pn);
        double prod2 = dot(rv, pn);
        double prod3 = prod1 / prod2;
        return sub(rp, mul(rv, prod3));
    }

    static void main() {
        Vector rv = new Vector(0.0, -1.0, -1.0);
        Vector rp = new Vector(0.0, 0.0, 10.0);
        Vector pn = new Vector(0.0, 0.0, 1.0);
        Vector pp = new Vector(0.0, 0.0, 5.0);
        Vector ip = intersectPoint(rv, rp, pn, pp);
        System.out.println("The ray intersects the plane at (" + String.valueOf(ip.x) + ", " + String.valueOf(ip.y) + ", " + String.valueOf(ip.z) + ")");
    }
    public static void main(String[] args) {
        main();
    }
}
