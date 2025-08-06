public class Main {
    static int seed = 0;
    static int NEG_ONE;

    static int rand() {
        seed = Math.floorMod((seed * 1103515245 + 12345), (int)2147483648L);
        return seed;
    }

    static int randint(int a, int b) {
        int r = rand();
        return a + Math.floorMod(r, (b - a + 1));
    }

    static double random() {
        return (1.0 * rand()) / 2147483648.0;
    }

    static int[][] construct_highway(int number_of_cells, int frequency, int initial_speed, boolean random_frequency, boolean random_speed, int max_speed) {
        int[] row = ((int[])(new int[]{}));
        int i = 0;
        while (i < number_of_cells) {
            row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(-1)).toArray()));
            i = i + 1;
        }
        int[][] highway = ((int[][])(new int[][]{}));
        highway = ((int[][])(appendObj(highway, row)));
        i = 0;
        if (initial_speed < 0) {
            initial_speed = 0;
        }
        while (i < number_of_cells) {
            int speed = initial_speed;
            if (((Boolean)(random_speed))) {
                speed = randint(0, max_speed);
            }
highway[0][i] = speed;
            int step = frequency;
            if (((Boolean)(random_frequency))) {
                step = randint(1, max_speed * 2);
            }
            i = i + step;
        }
        return highway;
    }

    static int get_distance(int[] highway_now, int car_index) {
        int distance = 0;
        int i_1 = car_index + 1;
        while (i_1 < highway_now.length) {
            if (highway_now[i_1] > NEG_ONE) {
                return distance;
            }
            distance = distance + 1;
            i_1 = i_1 + 1;
        }
        return distance + get_distance(((int[])(highway_now)), -1);
    }

    static int[] update(int[] highway_now, double probability, int max_speed) {
        int number_of_cells = highway_now.length;
        int[] next_highway = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < number_of_cells) {
            next_highway = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(next_highway), java.util.stream.IntStream.of(-1)).toArray()));
            i_2 = i_2 + 1;
        }
        int car_index = 0;
        while (car_index < number_of_cells) {
            int speed_1 = highway_now[car_index];
            if (speed_1 > NEG_ONE) {
                int new_speed = speed_1 + 1;
                if (new_speed > max_speed) {
                    new_speed = max_speed;
                }
                int dn = get_distance(((int[])(highway_now)), car_index) - 1;
                if (new_speed > dn) {
                    new_speed = dn;
                }
                if (random() < probability) {
                    new_speed = new_speed - 1;
                    if (new_speed < 0) {
                        new_speed = 0;
                    }
                }
next_highway[car_index] = new_speed;
            }
            car_index = car_index + 1;
        }
        return next_highway;
    }

    static int[][] simulate(int[][] highway, int number_of_update, double probability, int max_speed) {
        int number_of_cells_1 = highway[0].length;
        int i_3 = 0;
        while (i_3 < number_of_update) {
            int[] next_speeds = ((int[])(update(((int[])(highway[i_3])), probability, max_speed)));
            int[] real_next = ((int[])(new int[]{}));
            int j = 0;
            while (j < number_of_cells_1) {
                real_next = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(real_next), java.util.stream.IntStream.of(-1)).toArray()));
                j = j + 1;
            }
            int k = 0;
            while (k < number_of_cells_1) {
                int speed_2 = next_speeds[k];
                if (speed_2 > NEG_ONE) {
                    int index = Math.floorMod((k + speed_2), number_of_cells_1);
real_next[index] = speed_2;
                }
                k = k + 1;
            }
            highway = ((int[][])(appendObj(highway, real_next)));
            i_3 = i_3 + 1;
        }
        return highway;
    }

    static void main() {
        int[][] ex1 = ((int[][])(simulate(((int[][])(construct_highway(6, 3, 0, false, false, 2))), 2, 0.0, 2)));
        System.out.println(_p(ex1));
        int[][] ex2 = ((int[][])(simulate(((int[][])(construct_highway(5, 2, -2, false, false, 2))), 3, 0.0, 2)));
        System.out.println(_p(ex2));
    }
    public static void main(String[] args) {
        seed = 1;
        NEG_ONE = -1;
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
