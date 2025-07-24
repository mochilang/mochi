public class Main {

    static String poly(int p) {
        String s = "";
        int coef = 1;
        int i = p;
        if (coef != 1) {
            s = s + String.valueOf(coef);
        }
        while (i > 0) {
            s = s + "x";
            if (i != 1) {
                s = s + "^" + String.valueOf(i);
            }
            coef = ((Number)((coef * i / (p - i + 1)))).intValue();
            int d = coef;
            if ((p - (i - 1)) % 2 == 1) {
                d = -d;
            }
            if (d < 0) {
                s = s + " - " + String.valueOf(-d);
            } else {
                s = s + " + " + String.valueOf(d);
            }
            i = i - 1;
        }
        if ((s.equals(""))) {
            s = "1";
        }
        return s;
    }

    static boolean aks(int n) {
        if (n < 2) {
            return false;
        }
        int c = n;
        int i = 1;
        while (i < n) {
            if (c % n != 0) {
                return false;
            }
            c = ((Number)((c * (n - i) / (i + 1)))).intValue();
            i = i + 1;
        }
        return true;
    }

    static void main() {
        int p = 0;
        while (p <= 7) {
            System.out.println(String.valueOf(p) + ":  " + poly(p));
            p = p + 1;
        }
        boolean first = true;
        p = 2;
        String line = "";
        while (p < 50) {
            if (aks(p)) {
                if (first) {
                    line = line + String.valueOf(p);
                    first = false;
                } else {
                    line = line + " " + String.valueOf(p);
                }
            }
            p = p + 1;
        }
        System.out.println(line);
    }
    public static void main(String[] args) {
        main();
    }
}
