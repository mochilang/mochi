public class Main {
    static String[] compassPoint = new String[]{"North", "North by east", "North-northeast", "Northeast by north", "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west"};
    static double[] headings = new double[]{0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38};
    static int i = 0;

    static String padLeft(String s, int w) {
        String res = "";
        int n = w - s.length();
        while (n > 0) {
            res = res + " ";
            n = n - 1;
        }
        return res + s;
    }

    static String padRight(String s, int w) {
        String out = s;
        int i = s.length();
        while (i < w) {
            out = out + " ";
            i = i + 1;
        }
        return out;
    }

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String format2(double f) {
        String s = String.valueOf(f);
        int idx = ((Number)(s.indexOf("."))).intValue();
        if (idx < 0) {
            s = s + ".00";
        } else {
            int need = idx + 3;
            if (s.length() > need) {
                s = s.substring(0, need);
            } else {
                while (s.length() < need) {
                    s = s + "0";
                }
            }
        }
        return s;
    }

    static int cpx(double h) {
        int x = ((Number)(((h / 11.25) + 0.5))).intValue();
        x = Math.floorMod(x, 32);
        if (x < 0) {
            x = x + 32;
        }
        return x;
    }

    static String degrees2compasspoint(double h) {
        return compassPoint[cpx(h)];
    }
    public static void main(String[] args) {
        System.out.println("Index  Compass point         Degree");
        while (i < headings.length) {
            double h = headings[i];
            int idx = Math.floorMod(i, 32) + 1;
            String cp = String.valueOf(degrees2compasspoint(h));
            System.out.println(String.valueOf(padLeft(String.valueOf(idx), 4)) + "   " + String.valueOf(padRight(cp, 19)) + " " + String.valueOf(format2(h)) + "°");
            i = i + 1;
        }
    }
}
