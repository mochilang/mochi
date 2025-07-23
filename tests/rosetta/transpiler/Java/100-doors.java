public class Main {
    static boolean[] doors = new boolean[]{};

    public static void main(String[] args) {
        for (int i = 0; i < 100; i++) {
            doors = appendBool(doors, false);
        }
        for (int pass = 1; pass < 101; pass++) {
            int idx = pass - 1;
            while (idx < 100) {
doors[idx] = !doors[idx];
                idx = idx + pass;
            }
        }
        for (int row = 0; row < 10; row++) {
            String line = "";
            for (int col = 0; col < 10; col++) {
                int idx = row * 10 + col;
                if (doors[idx]) {
                    line = line + "1";
                } else {
                    line = line + "0";
                }
                if (col < 9) {
                    line = line + " ";
                }
            }
            System.out.println(line);
        }
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
