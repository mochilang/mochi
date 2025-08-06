public class Main {
    static int WIDTH;
    static int HEIGHT;
    static int PREY_INITIAL_COUNT;
    static int PREY_REPRODUCTION_TIME;
    static int PREDATOR_INITIAL_COUNT;
    static int PREDATOR_REPRODUCTION_TIME;
    static int PREDATOR_INITIAL_ENERGY;
    static int PREDATOR_FOOD_VALUE;
    static int TYPE_PREY;
    static int TYPE_PREDATOR;
    static int seed = 0;
    static int[][] board_1 = new int[0][];
    static int[][] entities = new int[0][];
    static int[] dr;
    static int[] dc;
    static int t = 0;

    static int rand() {
        seed = Math.floorMod((seed * 1103515245 + 12345), (int)2147483648L);
        return seed;
    }

    static int rand_range(int max) {
        return Math.floorMod(rand(), max);
    }

    static int[] shuffle(int[] list_int) {
        int i = list_int.length - 1;
        while (i > 0) {
            int j = rand_range(i + 1);
            int tmp = list_int[i];
list_int[i] = list_int[j];
list_int[j] = tmp;
            i = i - 1;
        }
        return list_int;
    }

    static int[][] create_board() {
        int[][] board = ((int[][])(new int[][]{}));
        int r = 0;
        while (r < HEIGHT) {
            int[] row = ((int[])(new int[]{}));
            int c = 0;
            while (c < WIDTH) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                c = c + 1;
            }
            board = ((int[][])(appendObj(board, row)));
            r = r + 1;
        }
        return board;
    }

    static int[] create_prey(int r, int c) {
        return new int[]{TYPE_PREY, r, c, PREY_REPRODUCTION_TIME, 0, 1};
    }

    static int[] create_predator(int r, int c) {
        return new int[]{TYPE_PREDATOR, r, c, PREDATOR_REPRODUCTION_TIME, PREDATOR_INITIAL_ENERGY, 1};
    }

    static boolean empty_cell(int r, int c) {
        return board_1[r][c] == 0;
    }

    static void add_entity(int typ) {
        while (true) {
            int r_1 = rand_range(HEIGHT);
            int c_1 = rand_range(WIDTH);
            if (((Boolean)(empty_cell(r_1, c_1)))) {
                if (typ == TYPE_PREY) {
board_1[r_1][c_1] = 1;
                    entities = ((int[][])(appendObj(entities, create_prey(r_1, c_1))));
                } else {
board_1[r_1][c_1] = 2;
                    entities = ((int[][])(appendObj(entities, create_predator(r_1, c_1))));
                }
                return;
            }
        }
    }

    static void setup() {
        int i_1 = 0;
        while (i_1 < PREY_INITIAL_COUNT) {
            add_entity(TYPE_PREY);
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < PREDATOR_INITIAL_COUNT) {
            add_entity(TYPE_PREDATOR);
            i_1 = i_1 + 1;
        }
    }

    static boolean inside(int r, int c) {
        return r >= 0 && r < HEIGHT && c >= 0 && c < WIDTH;
    }

    static int find_prey(int r, int c) {
        int i_2 = 0;
        while (i_2 < entities.length) {
            int[] e = ((int[])(entities[i_2]));
            if (e[5] == 1 && e[0] == TYPE_PREY && e[1] == r && e[2] == c) {
                return i_2;
            }
            i_2 = i_2 + 1;
        }
        return -1;
    }

    static void step_world() {
        int i_3 = 0;
        while (i_3 < entities.length) {
            int[] e_1 = ((int[])(entities[i_3]));
            if (e_1[5] == 0) {
                i_3 = i_3 + 1;
                continue;
            }
            int typ = e_1[0];
            int row_1 = e_1[1];
            int col = e_1[2];
            int repro = e_1[3];
            int energy = e_1[4];
            int[] dirs = ((int[])(new int[]{0, 1, 2, 3}));
            dirs = ((int[])(shuffle(((int[])(dirs)))));
            boolean moved = false;
            int old_r = row_1;
            int old_c = col;
            if (typ == TYPE_PREDATOR) {
                int j_1 = 0;
                boolean ate = false;
                while (j_1 < 4) {
                    int d = dirs[j_1];
                    int nr = row_1 + dr[d];
                    int nc = col + dc[d];
                    if (((Boolean)(inside(nr, nc))) && board_1[nr][nc] == 1) {
                        int prey_index = find_prey(nr, nc);
                        if (prey_index >= 0) {
entities[prey_index][5] = 0;
                        }
board_1[nr][nc] = 2;
board_1[row_1][col] = 0;
e_1[1] = nr;
e_1[2] = nc;
e_1[4] = energy + PREDATOR_FOOD_VALUE - 1;
                        moved = true;
                        ate = true;
                        break;
                    }
                    j_1 = j_1 + 1;
                }
                if (!ate) {
                    j_1 = 0;
                    while (j_1 < 4) {
                        int d_1 = dirs[j_1];
                        int nr_1 = row_1 + dr[d_1];
                        int nc_1 = col + dc[d_1];
                        if (((Boolean)(inside(nr_1, nc_1))) && board_1[nr_1][nc_1] == 0) {
board_1[nr_1][nc_1] = 2;
board_1[row_1][col] = 0;
e_1[1] = nr_1;
e_1[2] = nc_1;
                            moved = true;
                            break;
                        }
                        j_1 = j_1 + 1;
                    }
e_1[4] = energy - 1;
                }
                if (e_1[4] <= 0) {
e_1[5] = 0;
board_1[e_1[1]][e_1[2]] = 0;
                }
            } else {
                int j_2 = 0;
                while (j_2 < 4) {
                    int d_2 = dirs[j_2];
                    int nr_2 = row_1 + dr[d_2];
                    int nc_2 = col + dc[d_2];
                    if (((Boolean)(inside(nr_2, nc_2))) && board_1[nr_2][nc_2] == 0) {
board_1[nr_2][nc_2] = 1;
board_1[row_1][col] = 0;
e_1[1] = nr_2;
e_1[2] = nc_2;
                        moved = true;
                        break;
                    }
                    j_2 = j_2 + 1;
                }
            }
            if (e_1[5] == 1) {
                if (moved && repro <= 0) {
                    if (typ == TYPE_PREY) {
board_1[old_r][old_c] = 1;
                        entities = ((int[][])(appendObj(entities, create_prey(old_r, old_c))));
e_1[3] = PREY_REPRODUCTION_TIME;
                    } else {
board_1[old_r][old_c] = 2;
                        entities = ((int[][])(appendObj(entities, create_predator(old_r, old_c))));
e_1[3] = PREDATOR_REPRODUCTION_TIME;
                    }
                } else {
e_1[3] = repro - 1;
                }
            }
            i_3 = i_3 + 1;
        }
        int[][] alive = ((int[][])(new int[][]{}));
        int k = 0;
        while (k < entities.length) {
            int[] e2 = ((int[])(entities[k]));
            if (e2[5] == 1) {
                alive = ((int[][])(appendObj(alive, e2)));
            }
            k = k + 1;
        }
        entities = ((int[][])(alive));
    }

    static int count_entities(int typ) {
        int cnt = 0;
        int i_4 = 0;
        while (i_4 < entities.length) {
            if (entities[i_4][0] == typ && entities[i_4][5] == 1) {
                cnt = cnt + 1;
            }
            i_4 = i_4 + 1;
        }
        return cnt;
    }
    public static void main(String[] args) {
        WIDTH = 10;
        HEIGHT = 10;
        PREY_INITIAL_COUNT = 20;
        PREY_REPRODUCTION_TIME = 5;
        PREDATOR_INITIAL_COUNT = 5;
        PREDATOR_REPRODUCTION_TIME = 20;
        PREDATOR_INITIAL_ENERGY = 15;
        PREDATOR_FOOD_VALUE = 5;
        TYPE_PREY = 0;
        TYPE_PREDATOR = 1;
        seed = 123456789;
        board_1 = ((int[][])(create_board()));
        entities = ((int[][])(new int[][]{}));
        dr = ((int[])(new int[]{-1, 0, 1, 0}));
        dc = ((int[])(new int[]{0, 1, 0, -1}));
        setup();
        t = 0;
        while (t < 10) {
            step_world();
            t = t + 1;
        }
        System.out.println("Prey: " + _p(count_entities(TYPE_PREY)));
        System.out.println("Predators: " + _p(count_entities(TYPE_PREDATOR)));
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
