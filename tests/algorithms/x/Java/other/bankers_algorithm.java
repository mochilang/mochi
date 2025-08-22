public class Main {
    static class State {
        long[] claim;
        long[][] alloc;
        long[][] max;
        State(long[] claim, long[][] alloc, long[][] max) {
            this.claim = claim;
            this.alloc = alloc;
            this.max = max;
        }
        State() {}
        @Override public String toString() {
            return String.format("{'claim': %s, 'alloc': %s, 'max': %s}", String.valueOf(claim), String.valueOf(alloc), String.valueOf(max));
        }
    }

    static long[] claim_vector = ((long[])(new long[]{8, 5, 9, 7}));
    static long[][] allocated_resources_table = ((long[][])(new long[][]{new long[]{2, 0, 1, 1}, new long[]{0, 1, 2, 1}, new long[]{4, 0, 0, 3}, new long[]{0, 2, 1, 0}, new long[]{1, 0, 3, 0}}));
    static long[][] maximum_claim_table = ((long[][])(new long[][]{new long[]{3, 2, 1, 4}, new long[]{0, 2, 5, 2}, new long[]{5, 1, 0, 5}, new long[]{1, 5, 3, 0}, new long[]{3, 0, 3, 3}}));

    static long[] processes_resource_summation(long[][] alloc) {
        long resources = (long)(alloc[(int)(0L)].length);
        long[] sums_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(resources)) {
            long total_1 = 0L;
            long j_1 = 0L;
            while ((long)(j_1) < (long)(alloc.length)) {
                total_1 = (long)((long)(total_1) + (long)(alloc[(int)((long)(j_1))][(int)((long)(i_1))]));
                j_1 = (long)((long)(j_1) + 1L);
            }
            sums_1 = ((long[])(appendLong(sums_1, (long)(total_1))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return sums_1;
    }

    static long[] available_resources(long[] claim, long[] alloc_sum) {
        long[] avail = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(claim.length)) {
            avail = ((long[])(appendLong(avail, (long)((long)(claim[(int)((long)(i_3))]) - (long)(alloc_sum[(int)((long)(i_3))])))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return avail;
    }

    static long[][] need(long[][] max, long[][] alloc) {
        long[][] needs = ((long[][])(new long[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(max.length)) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(max[(int)(0L)].length)) {
                row_1 = ((long[])(appendLong(row_1, (long)((long)(max[(int)((long)(i_5))][(int)((long)(j_3))]) - (long)(alloc[(int)((long)(i_5))][(int)((long)(j_3))])))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            needs = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(needs), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return needs;
    }

    static void pretty_print(long[] claim, long[][] alloc, long[][] max) {
        System.out.println("         Allocated Resource Table");
        long i_7 = 0L;
        while ((long)(i_7) < (long)(alloc.length)) {
            long[] row_3 = ((long[])(alloc[(int)((long)(i_7))]));
            String line_1 = "P" + _p((long)(i_7) + 1L) + "       ";
            long j_5 = 0L;
            while ((long)(j_5) < (long)(row_3.length)) {
                line_1 = line_1 + _p(_geti(row_3, ((Number)(j_5)).intValue()));
                if ((long)(j_5) < (long)((long)(row_3.length) - 1L)) {
                    line_1 = line_1 + "        ";
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            System.out.println(line_1);
            System.out.println("");
            i_7 = (long)((long)(i_7) + 1L);
        }
        System.out.println("         System Resource Table");
        i_7 = 0L;
        while ((long)(i_7) < (long)(max.length)) {
            long[] row_5 = ((long[])(max[(int)((long)(i_7))]));
            String line_3 = "P" + _p((long)(i_7) + 1L) + "       ";
            long j_7 = 0L;
            while ((long)(j_7) < (long)(row_5.length)) {
                line_3 = line_3 + _p(_geti(row_5, ((Number)(j_7)).intValue()));
                if ((long)(j_7) < (long)((long)(row_5.length) - 1L)) {
                    line_3 = line_3 + "        ";
                }
                j_7 = (long)((long)(j_7) + 1L);
            }
            System.out.println(line_3);
            System.out.println("");
            i_7 = (long)((long)(i_7) + 1L);
        }
        String usage_1 = "";
        i_7 = 0L;
        while ((long)(i_7) < (long)(claim.length)) {
            if ((long)(i_7) > 0L) {
                usage_1 = usage_1 + " ";
            }
            usage_1 = usage_1 + _p(_geti(claim, ((Number)(i_7)).intValue()));
            i_7 = (long)((long)(i_7) + 1L);
        }
        long[] alloc_sum_1 = ((long[])(processes_resource_summation(((long[][])(alloc)))));
        long[] avail_2 = ((long[])(available_resources(((long[])(claim)), ((long[])(alloc_sum_1)))));
        String avail_str_1 = "";
        i_7 = 0L;
        while ((long)(i_7) < (long)(avail_2.length)) {
            if ((long)(i_7) > 0L) {
                avail_str_1 = avail_str_1 + " ";
            }
            avail_str_1 = avail_str_1 + _p(_geti(avail_2, ((Number)(i_7)).intValue()));
            i_7 = (long)((long)(i_7) + 1L);
        }
        System.out.println("Current Usage by Active Processes: " + usage_1);
        System.out.println("Initial Available Resources:       " + avail_str_1);
    }

    static void bankers_algorithm(long[] claim, long[][] alloc, long[][] max) {
        long[][] need_list = ((long[][])(need(((long[][])(max)), ((long[][])(alloc)))));
        long[] alloc_sum_3 = ((long[])(processes_resource_summation(((long[][])(alloc)))));
        long[] avail_4 = ((long[])(available_resources(((long[])(claim)), ((long[])(alloc_sum_3)))));
        System.out.println("__________________________________________________");
        System.out.println("");
        boolean[] finished_1 = ((boolean[])(new boolean[]{}));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(need_list.length)) {
            finished_1 = ((boolean[])(appendBool(finished_1, false)));
            i_9 = (long)((long)(i_9) + 1L);
        }
        long remaining_1 = (long)(need_list.length);
        while ((long)(remaining_1) > 0L) {
            boolean safe_1 = false;
            long p_1 = 0L;
            while ((long)(p_1) < (long)(need_list.length)) {
                if (!(Boolean)finished_1[(int)((long)(p_1))]) {
                    boolean exec_1 = true;
                    long r_1 = 0L;
                    while ((long)(r_1) < (long)(avail_4.length)) {
                        if ((long)(need_list[(int)((long)(p_1))][(int)((long)(r_1))]) > (long)(avail_4[(int)((long)(r_1))])) {
                            exec_1 = false;
                            break;
                        }
                        r_1 = (long)((long)(r_1) + 1L);
                    }
                    if (exec_1) {
                        safe_1 = true;
                        System.out.println("Process " + _p((long)(p_1) + 1L) + " is executing.");
                        r_1 = 0L;
                        while ((long)(r_1) < (long)(avail_4.length)) {
avail_4[(int)((long)(r_1))] = (long)((long)(avail_4[(int)((long)(r_1))]) + (long)(alloc[(int)((long)(p_1))][(int)((long)(r_1))]));
                            r_1 = (long)((long)(r_1) + 1L);
                        }
                        String avail_str_3 = "";
                        r_1 = 0L;
                        while ((long)(r_1) < (long)(avail_4.length)) {
                            if ((long)(r_1) > 0L) {
                                avail_str_3 = avail_str_3 + " ";
                            }
                            avail_str_3 = avail_str_3 + _p(_geti(avail_4, ((Number)(r_1)).intValue()));
                            r_1 = (long)((long)(r_1) + 1L);
                        }
                        System.out.println("Updated available resource stack for processes: " + avail_str_3);
                        System.out.println("The process is in a safe state.");
                        System.out.println("");
finished_1[(int)((long)(p_1))] = true;
                        remaining_1 = (long)((long)(remaining_1) - 1L);
                    }
                }
                p_1 = (long)((long)(p_1) + 1L);
            }
            if (!safe_1) {
                System.out.println("System in unsafe state. Aborting...");
                System.out.println("");
                break;
            }
        }
    }
    public static void main(String[] args) {
        pretty_print(((long[])(claim_vector)), ((long[][])(allocated_resources_table)), ((long[][])(maximum_claim_table)));
        bankers_algorithm(((long[])(claim_vector)), ((long[][])(allocated_resources_table)), ((long[][])(maximum_claim_table)));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
