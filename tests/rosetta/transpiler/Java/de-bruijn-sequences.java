public class Main {

    static int[] dbRec(int k, int n, int t, int p, int[] a, int[] seq) {
        if (t > n) {
            if (Math.floorMod(n, p) == 0) {
                int j = 1;
                while (j <= p) {
                    seq = java.util.stream.IntStream.concat(java.util.Arrays.stream(seq), java.util.stream.IntStream.of(a[j])).toArray();
                    j = j + 1;
                }
            }
        } else {
a[t] = a[t - p];
            seq = dbRec(k, n, t + 1, p, a, seq);
            int j = a[t - p] + 1;
            while (j < k) {
a[t] = j;
                seq = dbRec(k, n, t + 1, t, a, seq);
                j = j + 1;
            }
        }
        return seq;
    }

    static String deBruijn(int k, int n) {
        String digits = "0123456789";
        String alphabet = digits;
        if (k < 10) {
            alphabet = digits.substring(0, k);
        }
        int[] a = new int[]{};
        int i = 0;
        while (i < k * n) {
            a = java.util.stream.IntStream.concat(java.util.Arrays.stream(a), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        int[] seq = new int[]{};
        seq = dbRec(k, n, 1, 1, a, seq);
        String b = "";
        int idx = 0;
        while (idx < seq.length) {
            b = b + alphabet.substring(seq[idx], seq[idx]+1);
            idx = idx + 1;
        }
        b = b + b.substring(0, n - 1);
        return b;
    }

    static boolean allDigits(String s) {
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if ((ch.compareTo("0") < 0) || (ch.compareTo("9") > 0)) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static int parseIntStr(String str) {
        int n = 0;
        int i = 0;
        while (i < str.length()) {
            n = n * 10 + (Integer.parseInt(str.substring(i, i + 1)));
            i = i + 1;
        }
        return n;
    }

    static void validate(String db) {
        int le = db.length();
        int[] found = new int[]{};
        int i = 0;
        while (i < 10000) {
            found = java.util.stream.IntStream.concat(java.util.Arrays.stream(found), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        int j = 0;
        while (j < le - 3) {
            String s = db.substring(j, j + 4);
            if (allDigits(s)) {
                int n = Integer.parseInt(s);
found[n] = found[n] + 1;
            }
            j = j + 1;
        }
        String[] errs = new String[]{};
        int k = 0;
        while (k < 10000) {
            if (found[k] == 0) {
                errs = java.util.stream.Stream.concat(java.util.Arrays.stream(errs), java.util.stream.Stream.of("    PIN number " + String.valueOf(padLeft(k, 4)) + " missing")).toArray(String[]::new);
            } else             if (found[k] > 1) {
                errs = java.util.stream.Stream.concat(java.util.Arrays.stream(errs), java.util.stream.Stream.of("    PIN number " + String.valueOf(padLeft(k, 4)) + " occurs " + String.valueOf(found[k]) + " times")).toArray(String[]::new);
            }
            k = k + 1;
        }
        int lerr = errs.length;
        if (lerr == 0) {
            System.out.println("  No errors found");
        } else {
            String pl = "s";
            if (lerr == 1) {
                pl = "";
            }
            System.out.println("  " + String.valueOf(lerr) + " error" + pl + " found:");
            String msg = String.valueOf(joinStr(errs, "\n"));
            System.out.println(msg);
        }
    }

    static String padLeft(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = "0" + s;
        }
        return s;
    }

    static String joinStr(String[] xs, String sep) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            if (i > 0) {
                res = res + sep;
            }
            res = res + xs[i];
            i = i + 1;
        }
        return res;
    }

    static String reverse(String s) {
        String out = "";
        int i = s.length() - 1;
        while (i >= 0) {
            out = out + s.substring(i, i + 1);
            i = i - 1;
        }
        return out;
    }

    static void main() {
        String db = String.valueOf(deBruijn(10, 4));
        int le = db.length();
        System.out.println("The length of the de Bruijn sequence is " + String.valueOf(le));
        System.out.println("\nThe first 130 digits of the de Bruijn sequence are:");
        System.out.println(db.substring(0, 130));
        System.out.println("\nThe last 130 digits of the de Bruijn sequence are:");
        System.out.println(db.substring(le - 130, db.length()));
        System.out.println("\nValidating the de Bruijn sequence:");
        validate(db);
        System.out.println("\nValidating the reversed de Bruijn sequence:");
        String dbr = String.valueOf(reverse(db));
        validate(dbr);
        db = db.substring(0, 4443) + "." + db.substring(4444, db.length());
        System.out.println("\nValidating the overlaid de Bruijn sequence:");
        validate(db);
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
}
