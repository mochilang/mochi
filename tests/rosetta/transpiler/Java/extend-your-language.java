public class Main {
    static class If2 {
        boolean cond1;
        boolean cond2;
        If2(boolean cond1, boolean cond2) {
            this.cond1 = cond1;
            this.cond2 = cond2;
        }
        @Override public String toString() {
            return String.format("{'cond1': %s, 'cond2': %s}", String.valueOf(cond1), String.valueOf(cond2));
        }
    }

    static int a;
    static int b;
    static If2 t;

    static If2 else1(If2 i, Runnable f) {
        if (i.cond1 && (i.cond2 == false)) {
            f.run();
        }
        return i;
    }

    static If2 else2(If2 i, Runnable f) {
        if (i.cond2 && (i.cond1 == false)) {
            f.run();
        }
        return i;
    }

    static If2 else0(If2 i, Runnable f) {
        if ((i.cond1 == false) && (i.cond2 == false)) {
            f.run();
        }
        return i;
    }

    static If2 if2(boolean cond1, boolean cond2, Runnable f) {
        if (((Boolean)(cond1)) && ((Boolean)(cond2))) {
            f.run();
        }
        return new If2(cond1, cond2);
    }
    public static void main(String[] args) {
        a = 0;
        b = 1;
        t = if2(a == 1, b == 3, () -> System.out.println("a = 1 and b = 3"));
        t = else1(t, () -> System.out.println("a = 1 and b <> 3"));
        t = else2(t, () -> System.out.println("a <> 1 and b = 3"));
        else0(t, () -> System.out.println("a <> 1 and b <> 3"));
        a = 1;
        b = 0;
        t = if2(a == 1, b == 3, () -> System.out.println("a = 1 and b = 3"));
        t = else0(t, () -> System.out.println("a <> 1 and b <> 3"));
        else1(t, () -> System.out.println("a = 1 and b <> 3"));
    }
}
