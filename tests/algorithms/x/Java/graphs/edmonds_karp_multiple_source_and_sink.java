public class Main {
    static long[][] graph;
    static long[] sources;
    static long[] sinks;
    static long result;

    static long push_relabel_max_flow(long[][] graph, long[] sources, long[] sinks) {
        if ((long)(sources.length) == 0L || (long)(sinks.length) == 0L) {
            return 0;
        }
        long[][] g_1 = ((long[][])(graph));
        long source_index_1 = (long)(sources[(int)((long)(0))]);
        long sink_index_1 = (long)(sinks[(int)((long)(0))]);
        if ((long)(sources.length) > 1L || (long)(sinks.length) > 1L) {
            long max_input_flow_1 = 0L;
            long i_1 = 0L;
            while ((long)(i_1) < (long)(sources.length)) {
                long j_2 = 0L;
                while ((long)(j_2) < (long)(g_1[(int)((long)(sources[(int)((long)(i_1))]))].length)) {
                    max_input_flow_1 = (long)((long)(max_input_flow_1) + (long)(g_1[(int)((long)(sources[(int)((long)(i_1))]))][(int)((long)(j_2))]));
                    j_2 = (long)((long)(j_2) + 1L);
                }
                i_1 = (long)((long)(i_1) + 1L);
            }
            long size_1 = (long)((long)(g_1.length) + 1L);
            long[][] new_graph_1 = ((long[][])(new long[][]{}));
            long[] zero_row_1 = ((long[])(new long[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(size_1)) {
                zero_row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(zero_row_1), java.util.stream.LongStream.of(0L)).toArray()));
                j_3 = (long)((long)(j_3) + 1L);
            }
            new_graph_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_graph_1), java.util.stream.Stream.of(new long[][]{zero_row_1})).toArray(long[][]::new)));
            long r_1 = 0L;
            while ((long)(r_1) < (long)(g_1.length)) {
                long[] row_1 = ((long[])(new long[]{0}));
                long c_1 = 0L;
                while ((long)(c_1) < (long)(g_1[(int)((long)(r_1))].length)) {
                    row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of((long)(g_1[(int)((long)(r_1))][(int)((long)(c_1))]))).toArray()));
                    c_1 = (long)((long)(c_1) + 1L);
                }
                new_graph_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_graph_1), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
                r_1 = (long)((long)(r_1) + 1L);
            }
            g_1 = ((long[][])(new_graph_1));
            i_1 = 0L;
            while ((long)(i_1) < (long)(sources.length)) {
g_1[(int)((long)(0))][(int)((long)((long)(sources[(int)((long)(i_1))]) + 1L))] = (long)(max_input_flow_1);
                i_1 = (long)((long)(i_1) + 1L);
            }
            source_index_1 = 0L;
            size_1 = (long)((long)(g_1.length) + 1L);
            new_graph_1 = ((long[][])(new long[][]{}));
            r_1 = 0L;
            while ((long)(r_1) < (long)(g_1.length)) {
                long[] row2_1 = ((long[])(g_1[(int)((long)(r_1))]));
                row2_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row2_1), java.util.stream.LongStream.of(0L)).toArray()));
                new_graph_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_graph_1), java.util.stream.Stream.of(new long[][]{row2_1})).toArray(long[][]::new)));
                r_1 = (long)((long)(r_1) + 1L);
            }
            long[] last_row_1 = ((long[])(new long[]{}));
            j_3 = 0L;
            while ((long)(j_3) < (long)(size_1)) {
                last_row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(last_row_1), java.util.stream.LongStream.of(0L)).toArray()));
                j_3 = (long)((long)(j_3) + 1L);
            }
            new_graph_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_graph_1), java.util.stream.Stream.of(new long[][]{last_row_1})).toArray(long[][]::new)));
            g_1 = ((long[][])(new_graph_1));
            i_1 = 0L;
            while ((long)(i_1) < (long)(sinks.length)) {
g_1[(int)((long)((long)(sinks[(int)((long)(i_1))]) + 1L))][(int)((long)((long)(size_1) - 1L))] = (long)(max_input_flow_1);
                i_1 = (long)((long)(i_1) + 1L);
            }
            sink_index_1 = (long)((long)(size_1) - 1L);
        }
        long n_1 = (long)(g_1.length);
        long[][] preflow_1 = ((long[][])(new long[][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n_1)) {
            long[] row_3 = ((long[])(new long[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(n_1)) {
                row_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_3), java.util.stream.LongStream.of(0L)).toArray()));
                j_5 = (long)((long)(j_5) + 1L);
            }
            preflow_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(preflow_1), java.util.stream.Stream.of(new long[][]{row_3})).toArray(long[][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long[] heights_1 = ((long[])(new long[]{}));
        i_3 = 0L;
        while ((long)(i_3) < (long)(n_1)) {
            heights_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(heights_1), java.util.stream.LongStream.of(0L)).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long[] excesses_1 = ((long[])(new long[]{}));
        i_3 = 0L;
        while ((long)(i_3) < (long)(n_1)) {
            excesses_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(excesses_1), java.util.stream.LongStream.of(0L)).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
heights_1[(int)((long)(source_index_1))] = (long)(n_1);
        i_3 = 0L;
        while ((long)(i_3) < (long)(n_1)) {
            long bandwidth_1 = (long)(g_1[(int)((long)(source_index_1))][(int)((long)(i_3))]);
preflow_1[(int)((long)(source_index_1))][(int)((long)(i_3))] = (long)((long)(preflow_1[(int)((long)(source_index_1))][(int)((long)(i_3))]) + (long)(bandwidth_1));
preflow_1[(int)((long)(i_3))][(int)((long)(source_index_1))] = (long)((long)(preflow_1[(int)((long)(i_3))][(int)((long)(source_index_1))]) - (long)(bandwidth_1));
excesses_1[(int)((long)(i_3))] = (long)((long)(excesses_1[(int)((long)(i_3))]) + (long)(bandwidth_1));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long[] vertices_list_1 = ((long[])(new long[]{}));
        i_3 = 0L;
        while ((long)(i_3) < (long)(n_1)) {
            if ((long)(i_3) != (long)(source_index_1) && (long)(i_3) != (long)(sink_index_1)) {
                vertices_list_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(vertices_list_1), java.util.stream.LongStream.of((long)(i_3))).toArray()));
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(vertices_list_1.length)) {
            long v_1 = (long)(vertices_list_1[(int)((long)(idx_1))]);
            long prev_height_1 = (long)(heights_1[(int)((long)(v_1))]);
            while ((long)(excesses_1[(int)((long)(v_1))]) > 0L) {
                long nb_1 = 0L;
                while ((long)(nb_1) < (long)(n_1)) {
                    if ((long)((long)(g_1[(int)((long)(v_1))][(int)((long)(nb_1))]) - (long)(preflow_1[(int)((long)(v_1))][(int)((long)(nb_1))])) > 0L && (long)(heights_1[(int)((long)(v_1))]) > (long)(heights_1[(int)((long)(nb_1))])) {
                        long delta_1 = (long)(excesses_1[(int)((long)(v_1))]);
                        long capacity_1 = (long)((long)(g_1[(int)((long)(v_1))][(int)((long)(nb_1))]) - (long)(preflow_1[(int)((long)(v_1))][(int)((long)(nb_1))]));
                        if ((long)(delta_1) > (long)(capacity_1)) {
                            delta_1 = (long)(capacity_1);
                        }
preflow_1[(int)((long)(v_1))][(int)((long)(nb_1))] = (long)((long)(preflow_1[(int)((long)(v_1))][(int)((long)(nb_1))]) + (long)(delta_1));
preflow_1[(int)((long)(nb_1))][(int)((long)(v_1))] = (long)((long)(preflow_1[(int)((long)(nb_1))][(int)((long)(v_1))]) - (long)(delta_1));
excesses_1[(int)((long)(v_1))] = (long)((long)(excesses_1[(int)((long)(v_1))]) - (long)(delta_1));
excesses_1[(int)((long)(nb_1))] = (long)((long)(excesses_1[(int)((long)(nb_1))]) + (long)(delta_1));
                    }
                    nb_1 = (long)((long)(nb_1) + 1L);
                }
                long min_height_1 = (long)(-1);
                nb_1 = 0L;
                while ((long)(nb_1) < (long)(n_1)) {
                    if ((long)((long)(g_1[(int)((long)(v_1))][(int)((long)(nb_1))]) - (long)(preflow_1[(int)((long)(v_1))][(int)((long)(nb_1))])) > 0L) {
                        if ((long)(min_height_1) == (long)((-1)) || (long)(heights_1[(int)((long)(nb_1))]) < (long)(min_height_1)) {
                            min_height_1 = (long)(heights_1[(int)((long)(nb_1))]);
                        }
                    }
                    nb_1 = (long)((long)(nb_1) + 1L);
                }
                if ((long)(min_height_1) != (long)((-1))) {
heights_1[(int)((long)(v_1))] = (long)((long)(min_height_1) + 1L);
                } else {
                    break;
                }
            }
            if ((long)(heights_1[(int)((long)(v_1))]) > (long)(prev_height_1)) {
                long vertex_1 = (long)(vertices_list_1[(int)((long)(idx_1))]);
                long j_7 = (long)(idx_1);
                while ((long)(j_7) > 0L) {
vertices_list_1[(int)((long)(j_7))] = (long)(vertices_list_1[(int)((long)((long)(j_7) - 1L))]);
                    j_7 = (long)((long)(j_7) - 1L);
                }
vertices_list_1[(int)((long)(0))] = (long)(vertex_1);
                idx_1 = 0L;
            } else {
                idx_1 = (long)((long)(idx_1) + 1L);
            }
        }
        long flow_1 = 0L;
        i_3 = 0L;
        while ((long)(i_3) < (long)(n_1)) {
            flow_1 = (long)((long)(flow_1) + (long)(preflow_1[(int)((long)(source_index_1))][(int)((long)(i_3))]));
            i_3 = (long)((long)(i_3) + 1L);
        }
        if ((long)(flow_1) < 0L) {
            flow_1 = (long)(-flow_1);
        }
        return flow_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((long[][])(new long[][]{new long[]{0, 7, 0, 0}, new long[]{0, 0, 6, 0}, new long[]{0, 0, 0, 8}, new long[]{9, 0, 0, 0}}));
            sources = ((long[])(new long[]{0}));
            sinks = ((long[])(new long[]{3}));
            result = (long)(push_relabel_max_flow(((long[][])(graph)), ((long[])(sources)), ((long[])(sinks))));
            System.out.println("maximum flow is " + _p(result));
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
}
