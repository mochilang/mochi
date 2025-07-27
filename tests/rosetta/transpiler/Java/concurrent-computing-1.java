public class Main {

    static String[] shuffle(String[] xs) {
        String[] arr = xs;
        int i = arr.length - 1;
        while (i > 0) {
            int j = Math.floorMod(_now(), (i + 1));
            String tmp = arr[i];
arr[i] = arr[j];
arr[j] = tmp;
            i = i - 1;
        }
        return arr;
    }
    public static void main(String[] args) {
        for (var w : shuffle(new String[]{"Enjoy", "Rosetta", "Code"})) {
            System.out.println(w);
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
}
