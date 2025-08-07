public class Main {
    static class State {
        int[] claim;
        int[][] alloc;
        int[][] max;
        State(int[] claim, int[][] alloc, int[][] max) {
            this.claim = claim;
            this.alloc = alloc;
            this.max = max;
        }
        State() {}
        @Override public String toString() {
            return String.format("{'claim': %s, 'alloc': %s, 'max': %s}", String.valueOf(claim), String.valueOf(alloc), String.valueOf(max));
        }
    }

    static int[] claim_vector = new int[0];
    static int[][] allocated_resources_table = new int[0][];
    static int[][] maximum_claim_table = new int[0][];

    static int[] processes_resource_summation(int[][] alloc) {
        int resources = alloc[0].length;
        int[] sums = ((int[])(new int[]{}));
        int i = 0;
        while (i < resources) {
            int total = 0;
            int j = 0;
            while (j < alloc.length) {
                total = total + alloc[j][i];
                j = j + 1;
            }
            sums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sums), java.util.stream.IntStream.of(total)).toArray()));
            i = i + 1;
        }
        return sums;
    }

    static int[] available_resources(int[] claim, int[] alloc_sum) {
        int[] avail = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < claim.length) {
            avail = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(avail), java.util.stream.IntStream.of(claim[i_1] - alloc_sum[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return avail;
    }

    static int[][] need(int[][] max, int[][] alloc) {
        int[][] needs = ((int[][])(new int[][]{}));
        int i_2 = 0;
        while (i_2 < max.length) {
            int[] row = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < max[0].length) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(max[i_2][j_1] - alloc[i_2][j_1])).toArray()));
                j_1 = j_1 + 1;
            }
            needs = ((int[][])(appendObj(needs, row)));
            i_2 = i_2 + 1;
        }
        return needs;
    }

    static void pretty_print(int[] claim, int[][] alloc, int[][] max) {
        System.out.println("         Allocated Resource Table");
        int i_3 = 0;
        while (i_3 < alloc.length) {
            int[] row_1 = ((int[])(alloc[i_3]));
            String line = "P" + _p(i_3 + 1) + "       ";
            int j_2 = 0;
            while (j_2 < row_1.length) {
                line = line + _p(_geti(row_1, j_2));
                if (j_2 < row_1.length - 1) {
                    line = line + "        ";
                }
                j_2 = j_2 + 1;
            }
            System.out.println(line);
            System.out.println("");
            i_3 = i_3 + 1;
        }
        System.out.println("         System Resource Table");
        i_3 = 0;
        while (i_3 < max.length) {
            int[] row_2 = ((int[])(max[i_3]));
            String line_1 = "P" + _p(i_3 + 1) + "       ";
            int j_3 = 0;
            while (j_3 < row_2.length) {
                line_1 = line_1 + _p(_geti(row_2, j_3));
                if (j_3 < row_2.length - 1) {
                    line_1 = line_1 + "        ";
                }
                j_3 = j_3 + 1;
            }
            System.out.println(line_1);
            System.out.println("");
            i_3 = i_3 + 1;
        }
        String usage = "";
        i_3 = 0;
        while (i_3 < claim.length) {
            if (i_3 > 0) {
                usage = usage + " ";
            }
            usage = usage + _p(_geti(claim, i_3));
            i_3 = i_3 + 1;
        }
        int[] alloc_sum = ((int[])(processes_resource_summation(((int[][])(alloc)))));
        int[] avail_1 = ((int[])(available_resources(((int[])(claim)), ((int[])(alloc_sum)))));
        String avail_str = "";
        i_3 = 0;
        while (i_3 < avail_1.length) {
            if (i_3 > 0) {
                avail_str = avail_str + " ";
            }
            avail_str = avail_str + _p(_geti(avail_1, i_3));
            i_3 = i_3 + 1;
        }
        System.out.println("Current Usage by Active Processes: " + usage);
        System.out.println("Initial Available Resources:       " + avail_str);
    }

    static void bankers_algorithm(int[] claim, int[][] alloc, int[][] max) {
        int[][] need_list = ((int[][])(need(((int[][])(max)), ((int[][])(alloc)))));
        int[] alloc_sum_1 = ((int[])(processes_resource_summation(((int[][])(alloc)))));
        int[] avail_2 = ((int[])(available_resources(((int[])(claim)), ((int[])(alloc_sum_1)))));
        System.out.println("__________________________________________________");
        System.out.println("");
        boolean[] finished = ((boolean[])(new boolean[]{}));
        int i_4 = 0;
        while (i_4 < need_list.length) {
            finished = ((boolean[])(appendBool(finished, false)));
            i_4 = i_4 + 1;
        }
        int remaining = need_list.length;
        while (remaining > 0) {
            boolean safe = false;
            int p = 0;
            while (p < need_list.length) {
                if (!(Boolean)finished[p]) {
                    boolean exec = true;
                    int r = 0;
                    while (r < avail_2.length) {
                        if (need_list[p][r] > avail_2[r]) {
                            exec = false;
                            break;
                        }
                        r = r + 1;
                    }
                    if (exec) {
                        safe = true;
                        System.out.println("Process " + _p(p + 1) + " is executing.");
                        r = 0;
                        while (r < avail_2.length) {
avail_2[r] = avail_2[r] + alloc[p][r];
                            r = r + 1;
                        }
                        String avail_str_1 = "";
                        r = 0;
                        while (r < avail_2.length) {
                            if (r > 0) {
                                avail_str_1 = avail_str_1 + " ";
                            }
                            avail_str_1 = avail_str_1 + _p(_geti(avail_2, r));
                            r = r + 1;
                        }
                        System.out.println("Updated available resource stack for processes: " + avail_str_1);
                        System.out.println("The process is in a safe state.");
                        System.out.println("");
finished[p] = true;
                        remaining = remaining - 1;
                    }
                }
                p = p + 1;
            }
            if (!safe) {
                System.out.println("System in unsafe state. Aborting...");
                System.out.println("");
                break;
            }
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            claim_vector = ((int[])(new int[]{8, 5, 9, 7}));
            allocated_resources_table = ((int[][])(new int[][]{new int[]{2, 0, 1, 1}, new int[]{0, 1, 2, 1}, new int[]{4, 0, 0, 3}, new int[]{0, 2, 1, 0}, new int[]{1, 0, 3, 0}}));
            maximum_claim_table = ((int[][])(new int[][]{new int[]{3, 2, 1, 4}, new int[]{0, 2, 5, 2}, new int[]{5, 1, 0, 5}, new int[]{1, 5, 3, 0}, new int[]{3, 0, 3, 3}}));
            pretty_print(((int[])(claim_vector)), ((int[][])(allocated_resources_table)), ((int[][])(maximum_claim_table)));
            bankers_algorithm(((int[])(claim_vector)), ((int[][])(allocated_resources_table)), ((int[][])(maximum_claim_table)));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
