public class Main {

    static int gcd(int a, int b) {
        int x = a;
        if (x < 0) {
            x = -x;
        }
        int y = b;
        if (y < 0) {
            y = -y;
        }
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        return x;
    }

    static java.util.Map<String,Integer> parseRational(String s) {
        int intPart = 0;
        int fracPart = 0;
        int denom = 1;
        boolean afterDot = false;
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if ((ch.equals("."))) {
                afterDot = true;
            } else {
                int d = Integer.parseInt(ch) - Integer.parseInt("0");
                if (!afterDot) {
                    intPart = intPart * 10 + d;
                } else {
                    fracPart = fracPart * 10 + d;
                    denom = denom * 10;
                }
            }
            i = i + 1;
        }
        int num = intPart * denom + fracPart;
        int g = gcd(num, denom);
        return new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("num", ((Number)((num / g))).intValue()), java.util.Map.entry("den", ((Number)((denom / g))).intValue())));
    }

    static void main() {
        String[] inputs = new String[]{"0.9054054", "0.518518", "0.75"};
        for (String s : inputs) {
            java.util.Map<String,Integer> r = parseRational(s);
            System.out.println(s + " = " + String.valueOf(((int)r.getOrDefault("num", 0))) + "/" + String.valueOf(((int)r.getOrDefault("den", 0))));
        }
    }
    public static void main(String[] args) {
        main();
    }
}
