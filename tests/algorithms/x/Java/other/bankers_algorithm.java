public class Main {
    static class State {
        java.math.BigInteger[] claim;
        java.math.BigInteger[][] alloc;
        java.math.BigInteger[][] max;
        State(java.math.BigInteger[] claim, java.math.BigInteger[][] alloc, java.math.BigInteger[][] max) {
            this.claim = claim;
            this.alloc = alloc;
            this.max = max;
        }
        State() {}
        @Override public String toString() {
            return String.format("{'claim': %s, 'alloc': %s, 'max': %s}", String.valueOf(claim), String.valueOf(alloc), String.valueOf(max));
        }
    }

    static java.math.BigInteger[] claim_vector = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(7)}));
    static java.math.BigInteger[][] allocated_resources_table = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(3)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0)}))}));
    static java.math.BigInteger[][] maximum_claim_table = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(4)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(2)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(3)}))}));

    static java.math.BigInteger[] processes_resource_summation(java.math.BigInteger[][] alloc) {
        java.math.BigInteger resources = new java.math.BigInteger(String.valueOf(alloc[_idx((alloc).length, 0L)].length));
        java.math.BigInteger[] sums_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(resources) < 0) {
            java.math.BigInteger total_1 = java.math.BigInteger.valueOf(0);
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(alloc.length))) < 0) {
                total_1 = new java.math.BigInteger(String.valueOf(total_1.add(alloc[_idx((alloc).length, ((java.math.BigInteger)(j_1)).longValue())][_idx((alloc[_idx((alloc).length, ((java.math.BigInteger)(j_1)).longValue())]).length, ((java.math.BigInteger)(i_1)).longValue())])));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            sums_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(sums_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(total_1)))).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(sums_1));
    }

    static java.math.BigInteger[] available_resources(java.math.BigInteger[] claim, java.math.BigInteger[] alloc_sum) {
        java.math.BigInteger[] avail = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(claim.length))) < 0) {
            avail = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(avail), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(claim[_idx((claim).length, ((java.math.BigInteger)(i_3)).longValue())].subtract(alloc_sum[_idx((alloc_sum).length, ((java.math.BigInteger)(i_3)).longValue())]))))).toArray(java.math.BigInteger[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(avail));
    }

    static java.math.BigInteger[][] need(java.math.BigInteger[][] max, java.math.BigInteger[][] alloc) {
        java.math.BigInteger[][] needs = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(max.length))) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
            while (j_3.compareTo(new java.math.BigInteger(String.valueOf(max[_idx((max).length, 0L)].length))) < 0) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(max[_idx((max).length, ((java.math.BigInteger)(i_5)).longValue())][_idx((max[_idx((max).length, ((java.math.BigInteger)(i_5)).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())].subtract(alloc[_idx((alloc).length, ((java.math.BigInteger)(i_5)).longValue())][_idx((alloc[_idx((alloc).length, ((java.math.BigInteger)(i_5)).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())]))))).toArray(java.math.BigInteger[]::new)));
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            needs = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(needs), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[][])(needs));
    }

    static void pretty_print(java.math.BigInteger[] claim, java.math.BigInteger[][] alloc, java.math.BigInteger[][] max) {
        System.out.println("         Allocated Resource Table");
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(alloc.length))) < 0) {
            java.math.BigInteger[] row_3 = ((java.math.BigInteger[])(alloc[_idx((alloc).length, ((java.math.BigInteger)(i_7)).longValue())]));
            String line_1 = "P" + _p(i_7.add(java.math.BigInteger.valueOf(1))) + "       ";
            java.math.BigInteger j_5 = java.math.BigInteger.valueOf(0);
            while (j_5.compareTo(new java.math.BigInteger(String.valueOf(row_3.length))) < 0) {
                line_1 = line_1 + _p(_geto(row_3, ((Number)(j_5)).intValue()));
                if (j_5.compareTo(new java.math.BigInteger(String.valueOf(row_3.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                    line_1 = line_1 + "        ";
                }
                j_5 = new java.math.BigInteger(String.valueOf(j_5.add(java.math.BigInteger.valueOf(1))));
            }
            System.out.println(line_1);
            System.out.println("");
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        System.out.println("         System Resource Table");
        i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(max.length))) < 0) {
            java.math.BigInteger[] row_5 = ((java.math.BigInteger[])(max[_idx((max).length, ((java.math.BigInteger)(i_7)).longValue())]));
            String line_3 = "P" + _p(i_7.add(java.math.BigInteger.valueOf(1))) + "       ";
            java.math.BigInteger j_7 = java.math.BigInteger.valueOf(0);
            while (j_7.compareTo(new java.math.BigInteger(String.valueOf(row_5.length))) < 0) {
                line_3 = line_3 + _p(_geto(row_5, ((Number)(j_7)).intValue()));
                if (j_7.compareTo(new java.math.BigInteger(String.valueOf(row_5.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                    line_3 = line_3 + "        ";
                }
                j_7 = new java.math.BigInteger(String.valueOf(j_7.add(java.math.BigInteger.valueOf(1))));
            }
            System.out.println(line_3);
            System.out.println("");
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        String usage_1 = "";
        i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(claim.length))) < 0) {
            if (i_7.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                usage_1 = usage_1 + " ";
            }
            usage_1 = usage_1 + _p(_geto(claim, ((Number)(i_7)).intValue()));
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] alloc_sum_1 = ((java.math.BigInteger[])(processes_resource_summation(((java.math.BigInteger[][])(alloc)))));
        java.math.BigInteger[] avail_2 = ((java.math.BigInteger[])(available_resources(((java.math.BigInteger[])(claim)), ((java.math.BigInteger[])(alloc_sum_1)))));
        String avail_str_1 = "";
        i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(avail_2.length))) < 0) {
            if (i_7.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                avail_str_1 = avail_str_1 + " ";
            }
            avail_str_1 = avail_str_1 + _p(_geto(avail_2, ((Number)(i_7)).intValue()));
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        System.out.println("Current Usage by Active Processes: " + usage_1);
        System.out.println("Initial Available Resources:       " + avail_str_1);
    }

    static void bankers_algorithm(java.math.BigInteger[] claim, java.math.BigInteger[][] alloc, java.math.BigInteger[][] max) {
        java.math.BigInteger[][] need_list = ((java.math.BigInteger[][])(need(((java.math.BigInteger[][])(max)), ((java.math.BigInteger[][])(alloc)))));
        java.math.BigInteger[] alloc_sum_3 = ((java.math.BigInteger[])(processes_resource_summation(((java.math.BigInteger[][])(alloc)))));
        java.math.BigInteger[] avail_4 = ((java.math.BigInteger[])(available_resources(((java.math.BigInteger[])(claim)), ((java.math.BigInteger[])(alloc_sum_3)))));
        System.out.println("__________________________________________________");
        System.out.println("");
        boolean[] finished_1 = ((boolean[])(new boolean[]{}));
        java.math.BigInteger i_9 = java.math.BigInteger.valueOf(0);
        while (i_9.compareTo(new java.math.BigInteger(String.valueOf(need_list.length))) < 0) {
            finished_1 = ((boolean[])(appendBool(finished_1, false)));
            i_9 = new java.math.BigInteger(String.valueOf(i_9.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger remaining_1 = new java.math.BigInteger(String.valueOf(need_list.length));
        while (remaining_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            boolean safe_1 = false;
            java.math.BigInteger p_1 = java.math.BigInteger.valueOf(0);
            while (p_1.compareTo(new java.math.BigInteger(String.valueOf(need_list.length))) < 0) {
                if (!(Boolean)finished_1[_idx((finished_1).length, ((java.math.BigInteger)(p_1)).longValue())]) {
                    boolean exec_1 = true;
                    java.math.BigInteger r_1 = java.math.BigInteger.valueOf(0);
                    while (r_1.compareTo(new java.math.BigInteger(String.valueOf(avail_4.length))) < 0) {
                        if (need_list[_idx((need_list).length, ((java.math.BigInteger)(p_1)).longValue())][_idx((need_list[_idx((need_list).length, ((java.math.BigInteger)(p_1)).longValue())]).length, ((java.math.BigInteger)(r_1)).longValue())].compareTo(avail_4[_idx((avail_4).length, ((java.math.BigInteger)(r_1)).longValue())]) > 0) {
                            exec_1 = false;
                            break;
                        }
                        r_1 = new java.math.BigInteger(String.valueOf(r_1.add(java.math.BigInteger.valueOf(1))));
                    }
                    if (exec_1) {
                        safe_1 = true;
                        System.out.println("Process " + _p(p_1.add(java.math.BigInteger.valueOf(1))) + " is executing.");
                        r_1 = java.math.BigInteger.valueOf(0);
                        while (r_1.compareTo(new java.math.BigInteger(String.valueOf(avail_4.length))) < 0) {
avail_4[(int)(((java.math.BigInteger)(r_1)).longValue())] = new java.math.BigInteger(String.valueOf(avail_4[_idx((avail_4).length, ((java.math.BigInteger)(r_1)).longValue())].add(alloc[_idx((alloc).length, ((java.math.BigInteger)(p_1)).longValue())][_idx((alloc[_idx((alloc).length, ((java.math.BigInteger)(p_1)).longValue())]).length, ((java.math.BigInteger)(r_1)).longValue())])));
                            r_1 = new java.math.BigInteger(String.valueOf(r_1.add(java.math.BigInteger.valueOf(1))));
                        }
                        String avail_str_3 = "";
                        r_1 = java.math.BigInteger.valueOf(0);
                        while (r_1.compareTo(new java.math.BigInteger(String.valueOf(avail_4.length))) < 0) {
                            if (r_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                                avail_str_3 = avail_str_3 + " ";
                            }
                            avail_str_3 = avail_str_3 + _p(_geto(avail_4, ((Number)(r_1)).intValue()));
                            r_1 = new java.math.BigInteger(String.valueOf(r_1.add(java.math.BigInteger.valueOf(1))));
                        }
                        System.out.println("Updated available resource stack for processes: " + avail_str_3);
                        System.out.println("The process is in a safe state.");
                        System.out.println("");
finished_1[(int)(((java.math.BigInteger)(p_1)).longValue())] = true;
                        remaining_1 = new java.math.BigInteger(String.valueOf(remaining_1.subtract(java.math.BigInteger.valueOf(1))));
                    }
                }
                p_1 = new java.math.BigInteger(String.valueOf(p_1.add(java.math.BigInteger.valueOf(1))));
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
            pretty_print(((java.math.BigInteger[])(claim_vector)), ((java.math.BigInteger[][])(allocated_resources_table)), ((java.math.BigInteger[][])(maximum_claim_table)));
            bankers_algorithm(((java.math.BigInteger[])(claim_vector)), ((java.math.BigInteger[][])(allocated_resources_table)), ((java.math.BigInteger[][])(maximum_claim_table)));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
