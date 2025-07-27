public class Main {
    static int seed = 1;
    static String suits = "CDHS";
    static String nums = "A23456789TJQK";

    static int rnd() {
        seed = Math.floorMod((seed * 214013 + 2531011), (int)2147483648L);
        return seed / 65536;
    }

    static int[] deal(int game) {
        seed = game;
        int[] deck = new int[]{};
        int i = 0;
        while (i < 52) {
            deck = java.util.stream.IntStream.concat(java.util.Arrays.stream(deck), java.util.stream.IntStream.of(51 - i)).toArray();
            i = i + 1;
        }
        i = 0;
        while (i < 51) {
            int j = 51 - (Math.floorMod(rnd(), (52 - i)));
            int tmp = deck[i];
deck[i] = deck[j];
deck[j] = tmp;
            i = i + 1;
        }
        return deck;
    }

    static void show(int[] cards) {
        int i = 0;
        while (i < cards.length) {
            int c = cards[i];
            stdout.write(" " + nums.substring(c / 4, c / 4 + 1) + suits.substring(Math.floorMod(c, 4), Math.floorMod(c, 4) + 1));
            if (Math.floorMod((i + 1), 8) == 0 || i + 1 == cards.length) {
                System.out.println("");
            }
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("");
            System.out.println("Game #1");
            show(deal(1));
            System.out.println("");
            System.out.println("Game #617");
            show(deal(617));
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
