public class Main {

    static int bitAt(int x, int idx) {
        int v = x;
        int i = 0;
        while (i < idx) {
            v = ((Number)((v / 2))).intValue();
            i = i + 1;
        }
        return Math.floorMod(v, 2);
    }

    static void outputState(String state) {
        String line = "";
        int i = 0;
        while (i < _runeLen(state)) {
            if ((state.substring(i, i + 1).equals("1"))) {
                line = line + "#";
            } else {
                line = line + " ";
            }
            i = i + 1;
        }
        System.out.println(line);
    }

    static String step(String state, int r) {
        int cells = _runeLen(state);
        String out = "";
        int i = 0;
        while (i < cells) {
            String l = state.substring(Math.floorMod((i - 1 + cells), cells), Math.floorMod((i - 1 + cells), cells) + 1);
            String c = state.substring(i, i + 1);
            String rt = state.substring(Math.floorMod((i + 1), cells), Math.floorMod((i + 1), cells) + 1);
            int idx = 0;
            if ((l.equals("1"))) {
                idx = idx + 4;
            }
            if ((c.equals("1"))) {
                idx = idx + 2;
            }
            if ((rt.equals("1"))) {
                idx = idx + 1;
            }
            if (bitAt(r, idx) == 1) {
                out = out + "1";
            } else {
                out = out + "0";
            }
            i = i + 1;
        }
        return out;
    }

    static void elem(int r, int cells, int generations, String state) {
        outputState(state);
        int g = 0;
        String s = state;
        while (g < generations) {
            s = String.valueOf(step(s, r));
            outputState(s);
            g = g + 1;
        }
    }

    static String randInit(int cells, int seed) {
        String s = "";
        int val = seed;
        int i = 0;
        while (i < cells) {
            val = Math.floorMod((val * 1664525 + 1013904223), 2147483647);
            if (Math.floorMod(val, 2) == 0) {
                s = s + "0";
            } else {
                s = s + "1";
            }
            i = i + 1;
        }
        return s;
    }

    static String singleInit(int cells) {
        String s = "";
        int i = 0;
        while (i < cells) {
            if (i == cells / 2) {
                s = s + "1";
            } else {
                s = s + "0";
            }
            i = i + 1;
        }
        return s;
    }

    static void main() {
        int cells = 20;
        int generations = 9;
        System.out.println("Single 1, rule 90:");
        String state = String.valueOf(singleInit(cells));
        elem(90, cells, generations, state);
        System.out.println("Random intial state, rule 30:");
        state = String.valueOf(randInit(cells, 3));
        elem(30, cells, generations, state);
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
