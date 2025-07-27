public class Main {

    static void main() {
        String[] philosophers = new String[]{"Aristotle", "Kant", "Spinoza", "Marx", "Russell"};
        int hunger = 3;
        System.out.println("table empty");
        for (String p : philosophers) {
            System.out.println(p + " seated");
        }
        int i = 0;
        while (i < philosophers.length) {
            String name = philosophers[i];
            int h = 0;
            while (h < hunger) {
                System.out.println(name + " hungry");
                System.out.println(name + " eating");
                System.out.println(name + " thinking");
                h = h + 1;
            }
            System.out.println(name + " satisfied");
            System.out.println(name + " left the table");
            i = i + 1;
        }
        System.out.println("table empty");
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
