public class Main {

    static void show(int n) {
        if (n == 1) {
            System.out.println("1: 1");
            return;
        }
        String out = String.valueOf(n) + ": ";
        String x = "";
        int m = n;
        int f = 2;
        while (m != 1) {
            if (Math.floorMod(m, f) == 0) {
                out = out + x + String.valueOf(f);
                x = "×";
                m = ((Number)((m / f))).intValue();
            } else {
                f = f + 1;
            }
        }
        System.out.println(out);
    }
    public static void main(String[] args) {
        show(1);
        for (int i = 2; i < 10; i++) {
            show(i);
        }
        System.out.println("...");
        for (int i = 2144; i < 2155; i++) {
            show(i);
        }
        System.out.println("...");
        for (int i = 9987; i < 10000; i++) {
            show(i);
        }
    }
}
