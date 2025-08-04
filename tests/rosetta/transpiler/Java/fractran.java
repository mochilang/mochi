public class Main {
    static class StepResult {
        java.math.BigInteger n;
        boolean ok;
        StepResult(java.math.BigInteger n, boolean ok) {
            this.n = n;
            this.ok = ok;
        }
        @Override public String toString() {
            return String.format("{'n': %s, 'ok': %s}", String.valueOf(n), String.valueOf(ok));
        }
    }


    static StepResult step(java.math.BigInteger n, java.math.BigInteger[][] program) {
        int i = 0;
        while (i < program.length) {
            java.math.BigInteger num = program[i][0];
            java.math.BigInteger den = program[i][1];
            if (n.remainder(den).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                n = (n.divide(den)).multiply(num);
                return new StepResult(n, true);
            }
            i = i + 1;
        }
        return new StepResult(n, false);
    }

    static void main() {
        java.math.BigInteger[][] program = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{new java.math.BigInteger[]{java.math.BigInteger.valueOf(17), java.math.BigInteger.valueOf(91)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(78), java.math.BigInteger.valueOf(85)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(19), java.math.BigInteger.valueOf(51)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(23), java.math.BigInteger.valueOf(38)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(29), java.math.BigInteger.valueOf(33)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(77), java.math.BigInteger.valueOf(29)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(95), java.math.BigInteger.valueOf(23)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(77), java.math.BigInteger.valueOf(19)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(17)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(11), java.math.BigInteger.valueOf(13)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(13), java.math.BigInteger.valueOf(11)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(15), java.math.BigInteger.valueOf(14)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(15), java.math.BigInteger.valueOf(2)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(55), java.math.BigInteger.valueOf(1)}}));
        java.math.BigInteger n = java.math.BigInteger.valueOf(2);
        int primes = 0;
        int count = 0;
        int limit = 1000000;
        java.math.BigInteger two = java.math.BigInteger.valueOf(2);
        String line = "";
        while (primes < 20 && count < limit) {
            StepResult res = step(n, ((java.math.BigInteger[][])(program)));
            n = new java.math.BigInteger(String.valueOf(res.n));
            if (!res.ok) {
                break;
            }
            java.math.BigInteger m = n;
            int pow = 0;
            while (m.remainder(two).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                m = m.divide(two);
                pow = pow + 1;
            }
            if (m.compareTo(java.math.BigInteger.valueOf(1)) == 0 && pow > 1) {
                line = line + _p(pow) + " ";
                primes = primes + 1;
            }
            count = count + 1;
        }
        if (_runeLen(line) > 0) {
            System.out.println(_substr(line, 0, _runeLen(line) - 1));
        } else {
            System.out.println("");
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
