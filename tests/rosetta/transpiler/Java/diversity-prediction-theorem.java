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
        String digits = String.valueOf(n);
        while (_runeLen(digits) <= prec) {
            digits = "0" + digits;
        }
        String intPart = _substr(digits, 0, _runeLen(digits) - prec);
        String fracPart = _substr(digits, _runeLen(digits) - prec, _runeLen(digits));
        return intPart + "." + fracPart;
    }

    static String padLeft(String s, int w) {
        String res = "";
        int n = w - _runeLen(s);
        while (n > 0) {
            res = res + " ";
            n = n - 1;
        }
        return res + s;
    }

    static double averageSquareDiff(double f, double[] preds) {
        double av = 0.0;
        int i = 0;
        while (i < preds.length) {
            av = av + (preds[i] - f) * (preds[i] - f);
            i = i + 1;
        }
        av = av / (((Number)(preds.length)).doubleValue());
        return av;
    }

    static double[] diversityTheorem(double truth, double[] preds) {
        double av = 0.0;
        int i = 0;
        while (i < preds.length) {
            av = av + preds[i];
            i = i + 1;
        }
        av = av / (((Number)(preds.length)).doubleValue());
        double avErr = averageSquareDiff(truth, preds);
        double crowdErr = (truth - av) * (truth - av);
        double div = averageSquareDiff(av, preds);
        return new double[]{avErr, crowdErr, div};
    }

    static void main() {
        double[][] predsArray = new double[][]{new double[]{48.0, 47.0, 51.0}, new double[]{48.0, 47.0, 51.0, 42.0}};
        double truth = 49.0;
        int i = 0;
        while (i < predsArray.length) {
            double[] preds = predsArray[i];
            double[] res = diversityTheorem(truth, preds);
            System.out.println("Average-error : " + String.valueOf(padLeft(String.valueOf(formatFloat(res[0], 3)), 6)));
            System.out.println("Crowd-error   : " + String.valueOf(padLeft(String.valueOf(formatFloat(res[1], 3)), 6)));
            System.out.println("Diversity     : " + String.valueOf(padLeft(String.valueOf(formatFloat(res[2], 3)), 6)));
            System.out.println("");
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
