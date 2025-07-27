public class Main {
    static int nMech = 5;
    static int detailsPerMech = 4;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (int mech = 1; mech < (nMech + 1); mech++) {
                int id = mech;
                System.out.println("worker " + String.valueOf(id) + " contracted to assemble " + String.valueOf(detailsPerMech) + " details");
                System.out.println("worker " + String.valueOf(id) + " enters shop");
                int d = 0;
                while (d < detailsPerMech) {
                    System.out.println("worker " + String.valueOf(id) + " assembling");
                    System.out.println("worker " + String.valueOf(id) + " completed detail");
                    d = d + 1;
                }
                System.out.println("worker " + String.valueOf(id) + " leaves shop");
                System.out.println("mechanism " + String.valueOf(mech) + " completed");
            }
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
