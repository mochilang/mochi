public class Main {
    static class Rational {
        int num;
        int denom;
        Rational(int num, int denom) {
            this.num = num;
            this.denom = denom;
        }
        @Override public String toString() {
            return String.format("{'num': %s, 'denom': %s}", String.valueOf(num), String.valueOf(denom));
        }
    }

    static int OP_ADD = 1;
    static int OP_SUB = 2;
    static int OP_MUL = 3;
    static int OP_DIV = 4;
    interface Expr {}

    static class Num implements Expr {
        Rational value;
        Num(Rational value) {
            this.value = value;
        }
        @Override public String toString() {
            return String.format("{'value': %s}", String.valueOf(value));
        }
    }

    static class Bin implements Expr {
        int op;
        Expr left;
        Expr right;
        Bin(int op, Expr left, Expr right) {
            this.op = op;
            this.left = left;
            this.right = right;
        }
        @Override public String toString() {
            return String.format("{'op': %s, 'left': %s, 'right': %s}", String.valueOf(op), String.valueOf(left), String.valueOf(right));
        }
    }

    static int n_cards = 4;
    static int goal = 24;
    static int digit_range = 9;

    static Rational binEval(int op, Expr l, Expr r) {
        Rational lv = exprEval(l);
        Rational rv = exprEval(r);
        if (op == OP_ADD) {
            return new Rational(((Number)(lv.num)).intValue() * ((Number)(rv.denom)).intValue() + ((Number)(lv.denom)).intValue() * ((Number)(rv.num)).intValue(), ((Number)(lv.denom)).intValue() * ((Number)(rv.denom)).intValue());
        }
        if (op == OP_SUB) {
            return new Rational(((Number)(lv.num)).intValue() * ((Number)(rv.denom)).intValue() - ((Number)(lv.denom)).intValue() * ((Number)(rv.num)).intValue(), ((Number)(lv.denom)).intValue() * ((Number)(rv.denom)).intValue());
        }
        if (op == OP_MUL) {
            return new Rational(((Number)(lv.num)).intValue() * ((Number)(rv.num)).intValue(), ((Number)(lv.denom)).intValue() * ((Number)(rv.denom)).intValue());
        }
        return new Rational(((Number)(lv.num)).intValue() * ((Number)(rv.denom)).intValue(), ((Number)(lv.denom)).intValue() * ((Number)(rv.num)).intValue());
    }

    static String binString(int op, Expr l, Expr r) {
        String ls = exprString(l);
        String rs = exprString(r);
        String opstr = "";
        if (op == OP_ADD) {
            opstr = " + ";
        } else         if (op == OP_SUB) {
            opstr = " - ";
        } else         if (op == OP_MUL) {
            opstr = " * ";
        } else {
            opstr = " / ";
        }
        return "(" + ls + opstr + rs + ")";
    }

    static Expr newNum(int n) {
        return new Num(new Rational(n, 1));
    }

    static Rational exprEval(Expr x) {
        return x instanceof Num ? ((Num)(x)).value : binEval(((Bin)(x)).op, ((Bin)(x)).left, ((Bin)(x)).right);
    }

    static String exprString(Expr x) {
        return x instanceof Num ? String.valueOf(((Num)(x)).value.num) : binString(((Bin)(x)).op, ((Bin)(x)).left, ((Bin)(x)).right);
    }

    static boolean solve(Expr[] xs) {
        if (xs.length == 1) {
            Rational f = exprEval(xs[0]);
            if (((Number)(f.denom)).intValue() != 0 && f.num == ((Number)(f.denom)).intValue() * goal) {
                System.out.println(exprString(xs[0]));
                return true;
            }
            return false;
        }
        int i = 0;
        while (i < xs.length) {
            int j = i + 1;
            while (j < xs.length) {
                Expr[] rest = new Expr[]{};
                int k = 0;
                while (k < xs.length) {
                    if (k != i && k != j) {
                        rest = java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(xs[k])).toArray(Expr[]::new);
                    }
                    k = k + 1;
                }
                Expr a = xs[i];
                Expr b = xs[j];
                Bin node = new Bin(OP_ADD, a, b);
                for (var op : new int[]{OP_ADD, OP_SUB, OP_MUL, OP_DIV}) {
                    node = new Bin(op, a, b);
                    if (solve(java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(node)).toArray(Expr[]::new))) {
                        return true;
                    }
                }
                node = new Bin(OP_SUB, b, a);
                if (solve(java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(node)).toArray(Expr[]::new))) {
                    return true;
                }
                node = new Bin(OP_DIV, b, a);
                if (solve(java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(node)).toArray(Expr[]::new))) {
                    return true;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return false;
    }

    static void main() {
        int iter = 0;
        while (iter < 10) {
            Expr[] cards = new Expr[]{};
            int i = 0;
            while (i < n_cards) {
                int n = (_now() % (digit_range - 1)) + 1;
                cards = java.util.stream.Stream.concat(java.util.Arrays.stream(cards), java.util.stream.Stream.of(newNum(n))).toArray(Expr[]::new);
                System.out.println(" " + String.valueOf(n));
                i = i + 1;
            }
            System.out.println(":  ");
            if (!(Boolean)solve(cards)) {
                System.out.println("No solution");
            }
            iter = iter + 1;
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
