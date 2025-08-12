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

    static long[] claim_vector = new long[0];
    static long[][] allocated_resources_table = new long[0][];
    static long[][] maximum_claim_table = new long[0][];

    static long[] processes_resource_summation(long[][] alloc) {
        long resources = (long)(((long[])_geto(alloc, (int)((long)(0)))).length);
        long[] sums_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(resources)) {
            long total_1 = 0L;
            long j_1 = 0L;
            while ((long)(j_1) < (long)(alloc.length)) {
                total_1 = (long)((long)(total_1) + _geti(((long[])_geto(alloc, (int)((long)(j_1)))), (int)((long)(i_1))));
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            sums_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(sums_1), java.util.stream.LongStream.of((long)(total_1))).toArray()));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return sums_1;
    }

    static long[] available_resources(long[] claim, long[] alloc_sum) {
        long[] avail = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(claim.length)) {
            avail = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(avail), java.util.stream.LongStream.of((long)(_geti(claim, (int)((long)(i_3))) - _geti(alloc_sum, (int)((long)(i_3)))))).toArray()));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return avail;
    }

    static long[][] need(long[][] max, long[][] alloc) {
        long[][] needs = ((long[][])(new long[][]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(max.length)) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(((long[])_geto(max, (int)((long)(0)))).length)) {
                row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of((long)(_geti(((long[])_geto(max, (int)((long)(i_5)))), (int)((long)(j_3))) - _geti(((long[])_geto(alloc, (int)((long)(i_5)))), (int)((long)(j_3)))))).toArray()));
                j_3 = (long)((long)(j_3) + (long)(1));
            }
            needs = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(needs), java.util.stream.Stream.of(row_1)).toArray(long[][]::new)));
            i_5 = (long)((long)(i_5) + (long)(1));
        }
        return needs;
    }

    static void pretty_print(long[] claim, long[][] alloc, long[][] max) {
        System.out.println("         Allocated Resource Table");
        long i_7 = 0L;
        while ((long)(i_7) < (long)(alloc.length)) {
            long[] row_3 = ((long[])(((long[])_geto(alloc, (int)((long)(i_7))))));
            String line_1 = "P" + _p((long)(i_7) + (long)(1)) + "       ";
            long j_5 = 0L;
            while ((long)(j_5) < (long)(row_3.length)) {
                line_1 = line_1 + _p(_geti(row_3, ((Number)(j_5)).intValue()));
                if ((long)(j_5) < (long)((long)(row_3.length) - (long)(1))) {
                    line_1 = line_1 + "        ";
                }
                j_5 = (long)((long)(j_5) + (long)(1));
            }
            System.out.println(line_1);
            System.out.println("");
            i_7 = (long)((long)(i_7) + (long)(1));
        }
        System.out.println("         System Resource Table");
        i_7 = (long)(0);
        while ((long)(i_7) < (long)(max.length)) {
            long[] row_5 = ((long[])(((long[])_geto(max, (int)((long)(i_7))))));
            String line_3 = "P" + _p((long)(i_7) + (long)(1)) + "       ";
            long j_7 = 0L;
            while ((long)(j_7) < (long)(row_5.length)) {
                line_3 = line_3 + _p(_geti(row_5, ((Number)(j_7)).intValue()));
                if ((long)(j_7) < (long)((long)(row_5.length) - (long)(1))) {
                    line_3 = line_3 + "        ";
                }
                j_7 = (long)((long)(j_7) + (long)(1));
            }
            System.out.println(line_3);
            System.out.println("");
            i_7 = (long)((long)(i_7) + (long)(1));
        }
        String usage_1 = "";
        i_7 = (long)(0);
        while ((long)(i_7) < (long)(claim.length)) {
            if ((long)(i_7) > (long)(0)) {
                usage_1 = usage_1 + " ";
            }
            usage_1 = usage_1 + _p(_geti(claim, ((Number)(i_7)).intValue()));
            i_7 = (long)((long)(i_7) + (long)(1));
        }
        long[] alloc_sum_1 = ((long[])(processes_resource_summation(((long[][])(alloc)))));
        long[] avail_2 = ((long[])(available_resources(((long[])(claim)), ((long[])(alloc_sum_1)))));
        String avail_str_1 = "";
        i_7 = (long)(0);
        while ((long)(i_7) < (long)(avail_2.length)) {
            if ((long)(i_7) > (long)(0)) {
                avail_str_1 = avail_str_1 + " ";
            }
            avail_str_1 = avail_str_1 + _p(_geti(avail_2, ((Number)(i_7)).intValue()));
            i_7 = (long)((long)(i_7) + (long)(1));
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
            i_9 = (long)((long)(i_9) + (long)(1));
        }
        long remaining_1 = (long)(need_list.length);
        while ((long)(remaining_1) > (long)(0)) {
            boolean safe_1 = false;
            long p_1 = 0L;
            while ((long)(p_1) < (long)(need_list.length)) {
                if (!(Boolean)_getb(finished_1, (int)((long)(p_1)))) {
                    boolean exec_1 = true;
                    long r_1 = 0L;
                    while ((long)(r_1) < (long)(avail_4.length)) {
                        if ((long)(_geti(((long[])_geto(need_list, (int)((long)(p_1)))), (int)((long)(r_1)))) > (long)(_geti(avail_4, (int)((long)(r_1))))) {
                            exec_1 = false;
                            break;
                        }
                        r_1 = (long)((long)(r_1) + (long)(1));
                    }
                    if (exec_1) {
                        safe_1 = true;
                        System.out.println("Process " + _p((long)(p_1) + (long)(1)) + " is executing.");
                        r_1 = (long)(0);
                        while ((long)(r_1) < (long)(avail_4.length)) {
avail_4[(int)((long)(r_1))] = (long)((long)(_geti(avail_4, (int)((long)(r_1)))) + _geti(((long[])_geto(alloc, (int)((long)(p_1)))), (int)((long)(r_1))));
                            r_1 = (long)((long)(r_1) + (long)(1));
                        }
                        String avail_str_3 = "";
                        r_1 = (long)(0);
                        while ((long)(r_1) < (long)(avail_4.length)) {
                            if ((long)(r_1) > (long)(0)) {
                                avail_str_3 = avail_str_3 + " ";
                            }
                            avail_str_3 = avail_str_3 + _p(_geti(avail_4, ((Number)(r_1)).intValue()));
                            r_1 = (long)((long)(r_1) + (long)(1));
                        }
                        System.out.println("Updated available resource stack for processes: " + avail_str_3);
                        System.out.println("The process is in a safe state.");
                        System.out.println("");
finished_1[(int)((long)(p_1))] = true;
                        remaining_1 = (long)((long)(remaining_1) - (long)(1));
                    }
                }
                p_1 = (long)((long)(p_1) + (long)(1));
            }
            if (!safe_1) {
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
            claim_vector = ((long[])(new long[]{8, 5, 9, 7}));
            allocated_resources_table = ((long[][])(new long[][]{new long[]{2, 0, 1, 1}, new long[]{0, 1, 2, 1}, new long[]{4, 0, 0, 3}, new long[]{0, 2, 1, 0}, new long[]{1, 0, 3, 0}}));
            maximum_claim_table = ((long[][])(new long[][]{new long[]{3, 2, 1, 4}, new long[]{0, 2, 5, 2}, new long[]{5, 1, 0, 5}, new long[]{1, 5, 3, 0}, new long[]{3, 0, 3, 3}}));
            pretty_print(((long[])(claim_vector)), ((long[][])(allocated_resources_table)), ((long[][])(maximum_claim_table)));
            bankers_algorithm(((long[])(claim_vector)), ((long[][])(allocated_resources_table)), ((long[][])(maximum_claim_table)));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static long _geti(long[] a, int i) {
        if (a == null) return 0L;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0L;
        return a[i];
    }

    static boolean _getb(boolean[] a, int i) {
        if (a == null) return false;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return false;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
