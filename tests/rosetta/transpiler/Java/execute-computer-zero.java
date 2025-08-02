public class Main {

    static int floorMod(int a, int b) {
        int r = Math.floorMod(a, b);
        if (r < 0) {
            r = r + b;
        }
        return r;
    }

    static int run(int[] bc) {
        int acc = 0;
        int pc = 0;
        while (pc < 32) {
            int op = bc[pc] / 32;
            int arg = Math.floorMod(bc[pc], 32);
            pc = pc + 1;
            if (op == 0) {
            } else             if (op == 1) {
                acc = bc[arg];
            } else             if (op == 2) {
bc[arg] = acc;
            } else             if (op == 3) {
                acc = floorMod(acc + bc[arg], 256);
            } else             if (op == 4) {
                acc = floorMod(acc - bc[arg], 256);
            } else             if (op == 5) {
                if (acc == 0) {
                    pc = arg;
                }
            } else             if (op == 6) {
                pc = arg;
            } else             if (op == 7) {
                break;
            } else {
                break;
            }
        }
        return acc;
    }

    static void main() {
        int[][] programs = new int[][]{new int[]{35, 100, 224, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, new int[]{44, 106, 76, 43, 141, 75, 168, 192, 44, 224, 8, 7, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, new int[]{46, 79, 109, 78, 47, 77, 48, 145, 171, 80, 192, 46, 224, 1, 1, 0, 8, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, new int[]{45, 111, 69, 112, 71, 0, 78, 0, 171, 79, 192, 46, 224, 32, 0, 28, 1, 0, 0, 0, 6, 0, 2, 26, 5, 20, 3, 30, 1, 22, 4, 24}, new int[]{35, 132, 224, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, new int[]{35, 132, 224, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, new int[]{35, 100, 224, 1, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
        int i = 0;
        while (i < programs.length) {
            int res = run(programs[i]);
            System.out.println(_p(res));
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
