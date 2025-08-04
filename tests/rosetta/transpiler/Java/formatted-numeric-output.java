public class Main {

    static double pow10(int n) {
        double r = 1.0;
        int i = 0;
        while (i < n) {
            r = r * 10.0;
            i = i + 1;
        }
        return r;
    }

    static String formatFloat(double f, int prec) {
        double scale = pow10(prec);
        double scaled = (f * scale) + 0.5;
        int n = (((Number)(scaled)).intValue());
        String digits = _p(n);
        while (_runeLen(digits) <= prec) {
            digits = "0" + digits;
        }
        String intPart = _substr(digits, 0, _runeLen(digits) - prec);
        String fracPart = _substr(digits, _runeLen(digits) - prec, _runeLen(digits));
        return intPart + "." + fracPart;
    }

    static String padLeftZeros(String s, int width) {
        String out = s;
        while (_runeLen(out) < width) {
            out = "0" + out;
        }
        return out;
    }
    public static void main(String[] args) {
        System.out.println(padLeftZeros(String.valueOf(formatFloat(7.125, 3)), 9));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
