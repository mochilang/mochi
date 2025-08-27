public class Main {
    static java.util.Map<String,java.math.BigInteger> digits;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static java.math.BigInteger parseInt(String s) {
        java.math.BigInteger i = java.math.BigInteger.valueOf(0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(0);
        while (i.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            n_1 = n_1.multiply(java.math.BigInteger.valueOf(10)).add(new java.math.BigInteger(String.valueOf((((Number)(((java.math.BigInteger)(digits).get(_substr(s, (int)(((java.math.BigInteger)(i)).longValue()), (int)(((java.math.BigInteger)(i.add(java.math.BigInteger.valueOf(1)))).longValue())))))).intValue()))));
            i = i.add(java.math.BigInteger.valueOf(1));
        }
        return n_1;
    }

    static String[] split(String s) {
        String[] parts = ((String[])(new String[]{}));
        String cur_1 = "";
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            String ch_1 = _substr(s, (int)(((java.math.BigInteger)(i_2)).longValue()), (int)(((java.math.BigInteger)(i_2.add(java.math.BigInteger.valueOf(1)))).longValue()));
            if ((String.valueOf(ch_1).equals(String.valueOf(" ")))) {
                if (new java.math.BigInteger(String.valueOf(_runeLen(cur_1))).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                    parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
                    cur_1 = "";
                }
            } else {
                cur_1 = cur_1 + ch_1;
            }
            i_2 = i_2.add(java.math.BigInteger.valueOf(1));
        }
        if (new java.math.BigInteger(String.valueOf(_runeLen(cur_1))).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
        }
        return ((String[])(parts));
    }

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)(-x);
        } else {
            return (double)(x);
        }
    }

    static double sqrt(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return (double)(0.0);
        }
        double r_1 = (double)(x);
        double prev_1 = (double)(0.0);
        while ((double)(absf((double)((double)(r_1) - (double)(prev_1)))) > (double)(1e-12)) {
            prev_1 = (double)(r_1);
            r_1 = (double)((double)(((double)(r_1) + (double)((double)(x) / (double)(r_1)))) / (double)(2.0));
        }
        return (double)(r_1);
    }

    static boolean[][] makeBoolGrid(java.math.BigInteger P, java.math.BigInteger Q) {
        boolean[][] g = ((boolean[][])(new boolean[][]{}));
        java.math.BigInteger i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(P) < 0) {
            boolean[] row_1 = ((boolean[])(new boolean[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(Q) < 0) {
                row_1 = ((boolean[])(appendBool(row_1, false)));
                j_1 = j_1.add(java.math.BigInteger.valueOf(1));
            }
            g = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(g), java.util.stream.Stream.of(new boolean[][]{((boolean[])(row_1))})).toArray(boolean[][]::new)));
            i_4 = i_4.add(java.math.BigInteger.valueOf(1));
        }
        return ((boolean[][])(g));
    }

    static boolean visible(java.math.BigInteger[][] grid, java.math.BigInteger P, java.math.BigInteger Q, java.math.BigInteger R, java.math.BigInteger C, java.math.BigInteger BR, java.math.BigInteger BC) {
        double X1 = (double)((double)((((Number)(C)).doubleValue())) - (double)(0.5));
        double Y1_1 = (double)((double)((((Number)(R)).doubleValue())) - (double)(0.5));
        double Z1_1 = (double)((double)((((Number)(grid[_idx((grid).length, (long)(((Number)((R.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][_idx((grid[_idx((grid).length, (long)(((Number)((R.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]).length, (long)(((Number)((C.subtract(java.math.BigInteger.valueOf(1))))).intValue()))])).doubleValue())) + (double)(0.5));
        double X2_1 = (double)((double)((((Number)(BC)).doubleValue())) - (double)(0.5));
        double Y2_1 = (double)((double)((((Number)(BR)).doubleValue())) - (double)(0.5));
        double Z2_1 = (double)((double)((((Number)(grid[_idx((grid).length, (long)(((Number)((BR.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][_idx((grid[_idx((grid).length, (long)(((Number)((BR.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]).length, (long)(((Number)((BC.subtract(java.math.BigInteger.valueOf(1))))).intValue()))])).doubleValue())) + (double)(0.5));
        double Dx_1 = (double)((double)(X2_1) - (double)(X1));
        double Dy_1 = (double)((double)(Y2_1) - (double)(Y1_1));
        double Dz_1 = (double)((double)(Z2_1) - (double)(Z1_1));
        double dist_1 = (double)(sqrt((double)((double)((double)((double)(Dx_1) * (double)(Dx_1)) + (double)((double)(Dy_1) * (double)(Dy_1))) + (double)((double)(Dz_1) * (double)(Dz_1)))));
        java.math.BigInteger steps_1 = new java.math.BigInteger(String.valueOf((((Number)(((double)(dist_1) * (double)(20.0)))).intValue()))).add(java.math.BigInteger.valueOf(1));
        double stepT_1 = (double)((double)(1.0) / (double)((((Number)(steps_1)).doubleValue())));
        java.math.BigInteger i_6 = java.math.BigInteger.valueOf(1);
        while (i_6.compareTo(steps_1) < 0) {
            double t_1 = (double)((double)(stepT_1) * (double)((((Number)(i_6)).doubleValue())));
            double X_1 = (double)((double)(X1) + (double)((double)(Dx_1) * (double)(t_1)));
            double Y_1 = (double)((double)(Y1_1) + (double)((double)(Dy_1) * (double)(t_1)));
            double Z_1 = (double)((double)(Z1_1) + (double)((double)(Dz_1) * (double)(t_1)));
            java.math.BigInteger rIdx_1 = new java.math.BigInteger(String.valueOf(((((Number)(Y_1)).intValue())))).add(java.math.BigInteger.valueOf(1));
            java.math.BigInteger cIdx_1 = new java.math.BigInteger(String.valueOf(((((Number)(X_1)).intValue())))).add(java.math.BigInteger.valueOf(1));
            if (rIdx_1.compareTo(java.math.BigInteger.valueOf(1)) < 0 || rIdx_1.compareTo(P) > 0 || cIdx_1.compareTo(java.math.BigInteger.valueOf(1)) < 0 || cIdx_1.compareTo(Q) > 0) {
                return false;
            }
            double H_1 = (double)(((Number)(grid[_idx((grid).length, (long)(((Number)((rIdx_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][_idx((grid[_idx((grid).length, (long)(((Number)((rIdx_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]).length, (long)(((Number)((cIdx_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))])).doubleValue());
            if ((double)(Z_1) <= (double)(H_1)) {
                return false;
            }
            i_6 = i_6.add(java.math.BigInteger.valueOf(1));
        }
        return true;
    }

    static boolean[][] computeVis(java.math.BigInteger[][] grid, java.math.BigInteger P, java.math.BigInteger Q, java.math.BigInteger BR, java.math.BigInteger BC) {
        boolean[][] vis = ((boolean[][])(makeBoolGrid(P, Q)));
        java.math.BigInteger r_3 = java.math.BigInteger.valueOf(1);
        while (r_3.compareTo(P) <= 0) {
            java.math.BigInteger c_1 = java.math.BigInteger.valueOf(1);
            while (c_1.compareTo(Q) <= 0) {
vis[_idx((vis).length, (long)(((Number)((r_3.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][(int)((long)(((Number)((c_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))] = visible(((java.math.BigInteger[][])(grid)), P, Q, r_3, c_1, BR, BC);
                c_1 = c_1.add(java.math.BigInteger.valueOf(1));
            }
            r_3 = r_3.add(java.math.BigInteger.valueOf(1));
        }
        return ((boolean[][])(vis));
    }

    static java.math.BigInteger bfs(java.math.BigInteger[][] grid, java.math.BigInteger P, java.math.BigInteger Q, java.math.BigInteger R1, java.math.BigInteger C1, java.math.BigInteger R2, java.math.BigInteger C2) {
        boolean[][] vis1 = ((boolean[][])(computeVis(((java.math.BigInteger[][])(grid)), P, Q, R1, C1)));
        boolean[][] vis2_1 = ((boolean[][])(computeVis(((java.math.BigInteger[][])(grid)), P, Q, R2, C2)));
        boolean[][] visited_1 = ((boolean[][])(makeBoolGrid(P, Q)));
        java.math.BigInteger[] qR_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] qC_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] qD_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        qR_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(qR_1), java.util.stream.Stream.of(R1)).toArray(java.math.BigInteger[]::new)));
        qC_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(qC_1), java.util.stream.Stream.of(C1)).toArray(java.math.BigInteger[]::new)));
        qD_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(qD_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
visited_1[_idx((visited_1).length, (long)(((Number)((R1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][(int)((long)(((Number)((C1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))] = true;
        java.math.BigInteger head_1 = java.math.BigInteger.valueOf(0);
        while (head_1.compareTo(new java.math.BigInteger(String.valueOf(qR_1.length))) < 0) {
            java.math.BigInteger r_5 = qR_1[_idx((qR_1).length, ((java.math.BigInteger)(head_1)).longValue())];
            java.math.BigInteger c_3 = qC_1[_idx((qC_1).length, ((java.math.BigInteger)(head_1)).longValue())];
            java.math.BigInteger d_1 = qD_1[_idx((qD_1).length, ((java.math.BigInteger)(head_1)).longValue())];
            if (r_5.compareTo(R2) == 0 && c_3.compareTo(C2) == 0) {
                return d_1;
            }
            java.math.BigInteger hr_1 = grid[_idx((grid).length, (long)(((Number)((r_5.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][_idx((grid[_idx((grid).length, (long)(((Number)((r_5.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]).length, (long)(((Number)((c_3.subtract(java.math.BigInteger.valueOf(1))))).intValue()))];
            java.math.BigInteger idx_1 = java.math.BigInteger.valueOf(0);
            while (idx_1.compareTo(java.math.BigInteger.valueOf(4)) < 0) {
                java.math.BigInteger nr_1 = r_5;
                java.math.BigInteger nc_1 = c_3;
                if (idx_1.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                    nr_1 = nr_1.subtract(java.math.BigInteger.valueOf(1));
                }
                if (idx_1.compareTo(java.math.BigInteger.valueOf(1)) == 0) {
                    nr_1 = nr_1.add(java.math.BigInteger.valueOf(1));
                }
                if (idx_1.compareTo(java.math.BigInteger.valueOf(2)) == 0) {
                    nc_1 = nc_1.subtract(java.math.BigInteger.valueOf(1));
                }
                if (idx_1.compareTo(java.math.BigInteger.valueOf(3)) == 0) {
                    nc_1 = nc_1.add(java.math.BigInteger.valueOf(1));
                }
                if (nr_1.compareTo(java.math.BigInteger.valueOf(1)) >= 0 && nr_1.compareTo(P) <= 0 && nc_1.compareTo(java.math.BigInteger.valueOf(1)) >= 0 && nc_1.compareTo(Q) <= 0) {
                    if (!visited_1[_idx((visited_1).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][_idx((visited_1[_idx((visited_1).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]).length, (long)(((Number)((nc_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]) {
                        java.math.BigInteger hn_1 = grid[_idx((grid).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][_idx((grid[_idx((grid).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]).length, (long)(((Number)((nc_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))];
                        java.math.BigInteger diff_1 = hn_1.subtract(hr_1);
                        if (diff_1.compareTo(java.math.BigInteger.valueOf(1)) <= 0 && diff_1.compareTo((java.math.BigInteger.valueOf(3)).negate()) >= 0) {
                            if (vis1[_idx((vis1).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][_idx((vis1[_idx((vis1).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]).length, (long)(((Number)((nc_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))] || vis2_1[_idx((vis2_1).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][_idx((vis2_1[_idx((vis2_1).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]).length, (long)(((Number)((nc_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))]) {
visited_1[_idx((visited_1).length, (long)(((Number)((nr_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))][(int)((long)(((Number)((nc_1.subtract(java.math.BigInteger.valueOf(1))))).intValue()))] = true;
                                qR_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(qR_1), java.util.stream.Stream.of(nr_1)).toArray(java.math.BigInteger[]::new)));
                                qC_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(qC_1), java.util.stream.Stream.of(nc_1)).toArray(java.math.BigInteger[]::new)));
                                qD_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(qD_1), java.util.stream.Stream.of(d_1.add(java.math.BigInteger.valueOf(1)))).toArray(java.math.BigInteger[]::new)));
                            }
                        }
                    }
                }
                idx_1 = idx_1.add(java.math.BigInteger.valueOf(1));
            }
            head_1 = head_1.add(java.math.BigInteger.valueOf(1));
        }
        return (java.math.BigInteger.valueOf(1)).negate();
    }

    static void main() {
        String tLine = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        if ((String.valueOf(tLine).equals(String.valueOf("")))) {
            return;
        }
        java.math.BigInteger t_3 = parseInt(tLine);
        java.math.BigInteger case_1 = java.math.BigInteger.valueOf(0);
        while (case_1.compareTo(t_3) < 0) {
            String line_1 = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            while ((String.valueOf(line_1).equals(String.valueOf("")))) {
                line_1 = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            }
            String[] pq_1 = ((String[])(split(line_1)));
            java.math.BigInteger P_1 = parseInt(pq_1[_idx((pq_1).length, 0L)]);
            java.math.BigInteger Q_1 = parseInt(pq_1[_idx((pq_1).length, 1L)]);
            java.math.BigInteger[][] grid_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
            java.math.BigInteger r_7 = java.math.BigInteger.valueOf(0);
            while (r_7.compareTo(P_1) < 0) {
                String[] rowParts_1 = ((String[])(split((_scanner.hasNextLine() ? _scanner.nextLine() : ""))));
                java.math.BigInteger[] row_3 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
                java.math.BigInteger c_5 = java.math.BigInteger.valueOf(0);
                while (c_5.compareTo(Q_1) < 0) {
                    row_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_3), java.util.stream.Stream.of(parseInt(rowParts_1[_idx((rowParts_1).length, ((java.math.BigInteger)(c_5)).longValue())]))).toArray(java.math.BigInteger[]::new)));
                    c_5 = c_5.add(java.math.BigInteger.valueOf(1));
                }
                grid_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(grid_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_3))})).toArray(java.math.BigInteger[][]::new)));
                r_7 = r_7.add(java.math.BigInteger.valueOf(1));
            }
            String[] coords_1 = ((String[])(split((_scanner.hasNextLine() ? _scanner.nextLine() : ""))));
            java.math.BigInteger R1_1 = parseInt(coords_1[_idx((coords_1).length, 0L)]);
            java.math.BigInteger C1_1 = parseInt(coords_1[_idx((coords_1).length, 1L)]);
            java.math.BigInteger R2_1 = parseInt(coords_1[_idx((coords_1).length, 2L)]);
            java.math.BigInteger C2_1 = parseInt(coords_1[_idx((coords_1).length, 3L)]);
            java.math.BigInteger res_1 = bfs(((java.math.BigInteger[][])(grid_1)), P_1, Q_1, R1_1, C1_1, R2_1, C2_1);
            if (res_1.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
                System.out.println("Mission impossible!");
            } else {
                System.out.println("The shortest path is " + _p(res_1) + " steps long.");
            }
            case_1 = case_1.add(java.math.BigInteger.valueOf(1));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            digits = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>(java.util.Map.of("0", java.math.BigInteger.valueOf(0), "1", java.math.BigInteger.valueOf(1), "2", java.math.BigInteger.valueOf(2), "3", java.math.BigInteger.valueOf(3), "4", java.math.BigInteger.valueOf(4), "5", java.math.BigInteger.valueOf(5), "6", java.math.BigInteger.valueOf(6), "7", java.math.BigInteger.valueOf(7), "8", java.math.BigInteger.valueOf(8), "9", java.math.BigInteger.valueOf(9)))));
            main();
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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

    static Object _getm(Object m, String k) {
        if (!(m instanceof java.util.Map<?,?>)) return null;
        return ((java.util.Map<?,?>)m).get(k);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
