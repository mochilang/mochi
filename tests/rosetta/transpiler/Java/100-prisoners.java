public class Main {

    static int[] shuffle(int[] xs) {
        int[] arr = xs;
        int i = 99;
        while (i > 0) {
            int j = _now() % (i + 1);
            int tmp = arr[i];
arr[i] = arr[j];
arr[j] = tmp;
            i = i - 1;
        }
        return arr;
    }

    static void doTrials(int trials, int np, String strategy) {
        int pardoned = 0;
        int t = 0;
        while (t < trials) {
            int[] drawers = new int[]{};
            int i = 0;
            while (i < 100) {
                drawers = java.util.stream.IntStream.concat(java.util.Arrays.stream(drawers), java.util.stream.IntStream.of(i)).toArray();
                i = i + 1;
            }
            drawers = shuffle(drawers);
            int p = 0;
            boolean success = true;
            while (p < np) {
                boolean found = false;
                if (strategy == "optimal") {
                    int prev = p;
                    int d = 0;
                    while (d < 50) {
                        int this_ = drawers[prev];
                        if (this_ == p) {
                            found = true;
                            break;
                        }
                        prev = this_;
                        d = d + 1;
                    }
                } else {
                    boolean[] opened = new boolean[]{};
                    int k = 0;
                    while (k < 100) {
                        opened = appendBool(opened, false);
                        k = k + 1;
                    }
                    int d = 0;
                    while (d < 50) {
                        int n = _now() % 100;
                        while (opened[n]) {
                            n = _now() % 100;
                        }
opened[n] = true;
                        if (drawers[n] == p) {
                            found = true;
                            break;
                        }
                        d = d + 1;
                    }
                }
                if (!(Boolean)found) {
                    success = false;
                    break;
                }
                p = p + 1;
            }
            if (success) {
                pardoned = pardoned + 1;
            }
            t = t + 1;
        }
        double rf = (pardoned) / (trials) * 100.0;
        System.out.println("  strategy = " + strategy + "  pardoned = " + String.valueOf(pardoned) + " relative frequency = " + String.valueOf(rf) + "%");
    }

    static void main() {
        int trials = 1000;
        for (var np : new int[]{10, 100}) {
            System.out.println("Results from " + String.valueOf(trials) + " trials with " + String.valueOf(np) + " prisoners:\n");
            for (var strat : new String[]{"random", "optimal"}) {
                doTrials(trials, np, strat);
            }
        }
    }
    public static void main(String[] args) {
        main();
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
        return (int)System.currentTimeMillis();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
