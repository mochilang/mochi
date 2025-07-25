public class Main {
    static class Parser {
        String expr;
        int pos;
        Parser(String expr, int pos) {
            this.expr = expr;
            this.pos = pos;
        }
        @Override public String toString() {
            return String.format("{'expr': '%s', 'pos': %s}", String.valueOf(expr), String.valueOf(pos));
        }
    }

    static class Res {
        int v;
        Parser p;
        Res(int v, Parser p) {
            this.v = v;
            this.p = p;
        }
        @Override public String toString() {
            return String.format("{'v': %s, 'p': %s}", String.valueOf(v), String.valueOf(p));
        }
    }


    static Parser skipWS(Parser p) {
        int i = p.pos;
        while ((i < p.expr.length() && p.expr.substring(i, i + 1).equals(" "))) {
            i = i + 1;
        }
p.pos = i;
        return p;
    }

    static int parseIntStr(String str) {
        int i = 0;
        int n = 0;
        while (i < str.length()) {
            n = n * 10 + (Integer.parseInt(str.substring(i, i + 1))) - 48;
            i = i + 1;
        }
        return n;
    }

    static Res parseNumber(Parser p) {
        p = skipWS(p);
        int start = p.pos;
        while (p.pos < p.expr.length()) {
            String ch = p.expr.substring(p.pos, p.pos + 1);
            if (((ch.compareTo("0") >= 0) && ch.compareTo("9") <= 0)) {
p.pos = p.pos + 1;
            } else {
                break;
            }
        }
        String token = p.expr.substring(start, p.pos);
        return new Res(parseIntStr(token), p);
    }

    static Res parseFactor(Parser p) {
        p = skipWS(p);
        if ((p.pos < p.expr.length() && p.expr.substring(p.pos, p.pos + 1).equals("("))) {
p.pos = p.pos + 1;
            Res r = parseExpr(p);
            int v = r.v;
            p = r.p;
            p = skipWS(p);
            if ((p.pos < p.expr.length() && p.expr.substring(p.pos, p.pos + 1).equals(")"))) {
p.pos = p.pos + 1;
            }
            return new Res(v, p);
        }
        if ((p.pos < p.expr.length() && p.expr.substring(p.pos, p.pos + 1).equals("-"))) {
p.pos = p.pos + 1;
            Res r = parseFactor(p);
            int v = r.v;
            p = r.p;
            return new Res(-v, p);
        }
        return parseNumber(p);
    }

    static int powInt(int base, int exp) {
        int r = 1;
        int b = base;
        int e = exp;
        while (e > 0) {
            if (e % 2 == 1) {
                r = r * b;
            }
            b = b * b;
            e = e / ((Number)(2)).intValue();
        }
        return r;
    }

    static Res parsePower(Parser p) {
        Res r = parseFactor(p);
        int v = r.v;
        p = r.p;
        while (true) {
            p = skipWS(p);
            if ((p.pos < p.expr.length() && p.expr.substring(p.pos, p.pos + 1).equals("^"))) {
p.pos = p.pos + 1;
                Res r2 = parseFactor(p);
                int rhs = r2.v;
                p = r2.p;
                v = powInt(v, rhs);
            } else {
                break;
            }
        }
        return new Res(v, p);
    }

    static Res parseTerm(Parser p) {
        Res r = parsePower(p);
        int v = r.v;
        p = r.p;
        while (true) {
            p = skipWS(p);
            if (p.pos < p.expr.length()) {
                String op = p.expr.substring(p.pos, p.pos + 1);
                if ((op.equals("*"))) {
p.pos = p.pos + 1;
                    Res r2 = parsePower(p);
                    int rhs = r2.v;
                    p = r2.p;
                    v = v * rhs;
                    continue;
                }
                if ((op.equals("/"))) {
p.pos = p.pos + 1;
                    Res r2 = parsePower(p);
                    int rhs = r2.v;
                    p = r2.p;
                    v = v / ((Number)(rhs)).intValue();
                    continue;
                }
            }
            break;
        }
        return new Res(v, p);
    }

    static Res parseExpr(Parser p) {
        Res r = parseTerm(p);
        int v = r.v;
        p = r.p;
        while (true) {
            p = skipWS(p);
            if (p.pos < p.expr.length()) {
                String op = p.expr.substring(p.pos, p.pos + 1);
                if ((op.equals("+"))) {
p.pos = p.pos + 1;
                    Res r2 = parseTerm(p);
                    int rhs = r2.v;
                    p = r2.p;
                    v = v + rhs;
                    continue;
                }
                if ((op.equals("-"))) {
p.pos = p.pos + 1;
                    Res r2 = parseTerm(p);
                    int rhs = r2.v;
                    p = r2.p;
                    v = v - rhs;
                    continue;
                }
            }
            break;
        }
        return new Res(v, p);
    }

    static int evalExpr(String expr) {
        Parser p = new Parser(expr, 0);
        Res r = parseExpr(p);
        return r.v;
    }

    static void main() {
        String expr = "2*(3-1)+2*5";
        System.out.println(String.valueOf(expr + " = ") + String.valueOf(evalExpr(expr)));
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
