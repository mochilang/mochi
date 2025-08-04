public class Main {
    static class SumCarry {
        boolean s;
        boolean c;
        SumCarry(boolean s, boolean c) {
            this.s = s;
            this.c = c;
        }
        @Override public String toString() {
            return String.format("{'s': %s, 'c': %s}", String.valueOf(s), String.valueOf(c));
        }
    }

    static class Add4Result {
        boolean v;
        boolean s3;
        boolean s2;
        boolean s1;
        boolean s0;
        Add4Result(boolean v, boolean s3, boolean s2, boolean s1, boolean s0) {
            this.v = v;
            this.s3 = s3;
            this.s2 = s2;
            this.s1 = s1;
            this.s0 = s0;
        }
        @Override public String toString() {
            return String.format("{'v': %s, 's3': %s, 's2': %s, 's1': %s, 's0': %s}", String.valueOf(v), String.valueOf(s3), String.valueOf(s2), String.valueOf(s1), String.valueOf(s0));
        }
    }


    static boolean xor(boolean a, boolean b) {
        return (((Boolean)(a)) && (!(Boolean)b)) || ((!(Boolean)a) && ((Boolean)(b)));
    }

    static SumCarry ha(boolean a, boolean b) {
        return new SumCarry(xor(a, b), ((Boolean)(a)) && ((Boolean)(b)));
    }

    static SumCarry fa(boolean a, boolean b, boolean c0) {
        SumCarry r1 = ha(a, c0);
        SumCarry r2 = ha(r1.s, b);
        return new SumCarry(r2.s, r1.c || r2.c);
    }

    static Add4Result add4(boolean a3, boolean a2, boolean a1, boolean a0, boolean b3, boolean b2, boolean b1, boolean b0) {
        SumCarry r0 = fa(a0, b0, false);
        SumCarry r1_1 = fa(a1, b1, r0.c);
        SumCarry r2_1 = fa(a2, b2, r1_1.c);
        SumCarry r3 = fa(a3, b3, r2_1.c);
        return new Add4Result(r3.c, r3.s, r2_1.s, r1_1.s, r0.s);
    }

    static int b2i(boolean b) {
        if (((Boolean)(b))) {
            return 1;
        }
        return 0;
    }

    static void main() {
        Add4Result r = add4(true, false, true, false, true, false, false, true);
        System.out.println(_p(b2i(r.v)) + " " + _p(b2i(r.s3)) + " " + _p(b2i(r.s2)) + " " + _p(b2i(r.s1)) + " " + _p(b2i(r.s0)));
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
