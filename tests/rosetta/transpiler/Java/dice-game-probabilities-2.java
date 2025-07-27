public class Main {

    static int roll(int nDice, int nSides) {
        int sum = 0;
        int i = 0;
        while (i < nDice) {
            sum = sum + (Math.floorMod(_now(), nSides)) + 1;
            i = i + 1;
        }
        return sum;
    }

    static double beats(int n1, int s1, int n2, int s2, int trials) {
        int wins = 0;
        int i = 0;
        while (i < trials) {
            if (roll(n1, s1) > roll(n2, s2)) {
                wins = wins + 1;
            }
            i = i + 1;
        }
        return (((Number)(wins)).doubleValue()) / (((Number)(trials)).doubleValue());
    }
    public static void main(String[] args) {
        System.out.println(String.valueOf(beats(9, 4, 6, 6, 1000)));
        System.out.println(String.valueOf(beats(5, 10, 7, 6, 1000)));
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
