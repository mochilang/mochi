public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static String[] split(String s, String sep) {
        String[] parts = ((String[])(new String[]{}));
        String cur_1 = "";
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            if (new java.math.BigInteger(String.valueOf(_runeLen(sep))).compareTo(java.math.BigInteger.valueOf(0)) > 0 && i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep)))).compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) <= 0 && (String.valueOf(_substr(s, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep)))))).longValue()))).equals(String.valueOf(sep)))) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
                cur_1 = "";
                i_1 = i_1.add(new java.math.BigInteger(String.valueOf(_runeLen(sep))));
            } else {
                cur_1 = cur_1 + _substr(s, (int)(((java.math.BigInteger)(i_1)).longValue()), (int)(((java.math.BigInteger)(i_1.add(java.math.BigInteger.valueOf(1)))).longValue()));
                i_1 = i_1.add(java.math.BigInteger.valueOf(1));
            }
        }
        parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur_1)).toArray(String[]::new)));
        return ((String[])(parts));
    }

    static java.math.BigInteger[] parse_ints(String line) {
        String[] pieces = ((String[])(line.split(java.util.regex.Pattern.quote(" "))));
        java.math.BigInteger[] nums_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(pieces.length))) < 0) {
            String p_1 = pieces[_idx((pieces).length, ((java.math.BigInteger)(i_3)).longValue())];
            if (new java.math.BigInteger(String.valueOf(_runeLen(p_1))).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                nums_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nums_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(Integer.parseInt(p_1))))).toArray(java.math.BigInteger[]::new)));
            }
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        return ((java.math.BigInteger[])(nums_1));
    }

    static java.math.BigInteger[] sort_unique(java.math.BigInteger[] arr) {
        java.math.BigInteger i_4 = java.math.BigInteger.valueOf(1);
        while (i_4.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            java.math.BigInteger j_1 = i_4;
            while (j_1.compareTo(java.math.BigInteger.valueOf(0)) > 0 && arr[_idx((arr).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].compareTo(arr[_idx((arr).length, ((java.math.BigInteger)(j_1)).longValue())]) > 0) {
                java.math.BigInteger tmp_1 = arr[_idx((arr).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())];
arr[(int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())] = arr[_idx((arr).length, ((java.math.BigInteger)(j_1)).longValue())];
arr[(int)(((java.math.BigInteger)(j_1)).longValue())] = tmp_1;
                j_1 = j_1.subtract(java.math.BigInteger.valueOf(1));
            }
            i_4 = i_4.add(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger[] res_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            if (i_4.compareTo(java.math.BigInteger.valueOf(0)) == 0 || arr[_idx((arr).length, ((java.math.BigInteger)(i_4)).longValue())].compareTo(arr[_idx((arr).length, ((java.math.BigInteger)(i_4.subtract(java.math.BigInteger.valueOf(1)))).longValue())]) != 0) {
                res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(arr[_idx((arr).length, ((java.math.BigInteger)(i_4)).longValue())])).toArray(java.math.BigInteger[]::new)));
            }
            i_4 = i_4.add(java.math.BigInteger.valueOf(1));
        }
        return ((java.math.BigInteger[])(res_1));
    }

    static boolean pointInPoly(java.math.BigInteger[] xs, java.math.BigInteger[] ys, double px, double py) {
        boolean inside = false;
        java.math.BigInteger i_6 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger j_3 = new java.math.BigInteger(String.valueOf(xs.length)).subtract(java.math.BigInteger.valueOf(1));
        while (i_6.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            double xi_1 = (double)(((Number)(xs[_idx((xs).length, ((java.math.BigInteger)(i_6)).longValue())])).doubleValue());
            double yi_1 = (double)(((Number)(ys[_idx((ys).length, ((java.math.BigInteger)(i_6)).longValue())])).doubleValue());
            double xj_1 = (double)(((Number)(xs[_idx((xs).length, ((java.math.BigInteger)(j_3)).longValue())])).doubleValue());
            double yj_1 = (double)(((Number)(ys[_idx((ys).length, ((java.math.BigInteger)(j_3)).longValue())])).doubleValue());
            if ((((double)(yi_1) > (double)(py)) && ((double)(yj_1) <= (double)(py))) || (((double)(yj_1) > (double)(py)) && ((double)(yi_1) <= (double)(py)))) {
                double xint_1 = (double)((double)((double)((double)(((double)(xj_1) - (double)(xi_1))) * (double)(((double)(py) - (double)(yi_1)))) / (double)(((double)(yj_1) - (double)(yi_1)))) + (double)(xi_1));
                if ((double)(px) < (double)(xint_1)) {
                    inside = !inside;
                }
            }
            j_3 = i_6;
            i_6 = i_6.add(java.math.BigInteger.valueOf(1));
        }
        return inside;
    }

    static boolean[][][] make3DBool(java.math.BigInteger a, java.math.BigInteger b, java.math.BigInteger c) {
        boolean[][][] arr = ((boolean[][][])(new boolean[][][]{}));
        java.math.BigInteger i_8 = java.math.BigInteger.valueOf(0);
        while (i_8.compareTo(a) < 0) {
            boolean[][] plane_1 = ((boolean[][])(new boolean[][]{}));
            java.math.BigInteger j_5 = java.math.BigInteger.valueOf(0);
            while (j_5.compareTo(b) < 0) {
                boolean[] row_1 = ((boolean[])(new boolean[]{}));
                java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
                while (k_1.compareTo(c) < 0) {
                    row_1 = ((boolean[])(appendBool(row_1, false)));
                    k_1 = k_1.add(java.math.BigInteger.valueOf(1));
                }
                plane_1 = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(plane_1), java.util.stream.Stream.of(new boolean[][]{((boolean[])(row_1))})).toArray(boolean[][]::new)));
                j_5 = j_5.add(java.math.BigInteger.valueOf(1));
            }
            arr = ((boolean[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new boolean[][][]{((boolean[][])(plane_1))})).toArray(boolean[][][]::new)));
            i_8 = i_8.add(java.math.BigInteger.valueOf(1));
        }
        return ((boolean[][][])(arr));
    }

    static void main() {
        String tLine = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        if ((String.valueOf(tLine).equals(String.valueOf("")))) {
            return;
        }
        java.math.BigInteger t_1 = new java.math.BigInteger(String.valueOf(Integer.parseInt(tLine)));
        java.math.BigInteger case_1 = java.math.BigInteger.valueOf(0);
        while (case_1.compareTo(t_1) < 0) {
            String fLine_1 = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            java.math.BigInteger F_1 = new java.math.BigInteger(String.valueOf(Integer.parseInt(fLine_1)));
            java.math.BigInteger[] xs_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger[] ys_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger[] zs_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            xs_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(xs_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            xs_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(xs_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1001))).toArray(java.math.BigInteger[]::new)));
            ys_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ys_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            ys_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ys_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1001))).toArray(java.math.BigInteger[]::new)));
            zs_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(zs_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            zs_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(zs_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1001))).toArray(java.math.BigInteger[]::new)));
            java.math.BigInteger[] faceXCoord_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger[][] faceYPoly_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
            java.math.BigInteger[][] faceZPoly_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
            java.math.BigInteger i_10 = java.math.BigInteger.valueOf(0);
            while (i_10.compareTo(F_1) < 0) {
                String line_1 = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
                java.math.BigInteger[] nums_3 = ((java.math.BigInteger[])(parse_ints(line_1)));
                java.math.BigInteger P_1 = nums_3[_idx((nums_3).length, 0L)];
                java.math.BigInteger[] ptsX_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
                java.math.BigInteger[] ptsY_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
                java.math.BigInteger[] ptsZ_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
                java.math.BigInteger j_8 = java.math.BigInteger.valueOf(0);
                while (j_8.compareTo(P_1) < 0) {
                    java.math.BigInteger x_1 = nums_3[_idx((nums_3).length, ((java.math.BigInteger)(java.math.BigInteger.valueOf(1).add(java.math.BigInteger.valueOf(3).multiply(j_8)))).longValue())];
                    java.math.BigInteger y_1 = nums_3[_idx((nums_3).length, ((java.math.BigInteger)(java.math.BigInteger.valueOf(1).add(java.math.BigInteger.valueOf(3).multiply(j_8)).add(java.math.BigInteger.valueOf(1)))).longValue())];
                    java.math.BigInteger z_1 = nums_3[_idx((nums_3).length, ((java.math.BigInteger)(java.math.BigInteger.valueOf(1).add(java.math.BigInteger.valueOf(3).multiply(j_8)).add(java.math.BigInteger.valueOf(2)))).longValue())];
                    ptsX_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ptsX_1), java.util.stream.Stream.of(x_1)).toArray(java.math.BigInteger[]::new)));
                    ptsY_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ptsY_1), java.util.stream.Stream.of(y_1)).toArray(java.math.BigInteger[]::new)));
                    ptsZ_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ptsZ_1), java.util.stream.Stream.of(z_1)).toArray(java.math.BigInteger[]::new)));
                    xs_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(xs_1), java.util.stream.Stream.of(x_1)).toArray(java.math.BigInteger[]::new)));
                    ys_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ys_1), java.util.stream.Stream.of(y_1)).toArray(java.math.BigInteger[]::new)));
                    zs_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(zs_1), java.util.stream.Stream.of(z_1)).toArray(java.math.BigInteger[]::new)));
                    j_8 = j_8.add(java.math.BigInteger.valueOf(1));
                }
                boolean allSame_1 = true;
                j_8 = java.math.BigInteger.valueOf(1);
                while (j_8.compareTo(P_1) < 0) {
                    if (ptsX_1[_idx((ptsX_1).length, ((java.math.BigInteger)(j_8)).longValue())].compareTo(ptsX_1[_idx((ptsX_1).length, 0L)]) != 0) {
                        allSame_1 = false;
                    }
                    j_8 = j_8.add(java.math.BigInteger.valueOf(1));
                }
                if (allSame_1) {
                    faceXCoord_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(faceXCoord_1), java.util.stream.Stream.of(ptsX_1[_idx((ptsX_1).length, 0L)])).toArray(java.math.BigInteger[]::new)));
                    faceYPoly_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(faceYPoly_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(ptsY_1))})).toArray(java.math.BigInteger[][]::new)));
                    faceZPoly_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(faceZPoly_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(ptsZ_1))})).toArray(java.math.BigInteger[][]::new)));
                }
                i_10 = i_10.add(java.math.BigInteger.valueOf(1));
            }
            xs_1 = ((java.math.BigInteger[])(sort_unique(((java.math.BigInteger[])(xs_1)))));
            ys_1 = ((java.math.BigInteger[])(sort_unique(((java.math.BigInteger[])(ys_1)))));
            zs_1 = ((java.math.BigInteger[])(sort_unique(((java.math.BigInteger[])(zs_1)))));
            java.math.BigInteger nx_1 = new java.math.BigInteger(String.valueOf(xs_1.length)).subtract(java.math.BigInteger.valueOf(1));
            java.math.BigInteger ny_1 = new java.math.BigInteger(String.valueOf(ys_1.length)).subtract(java.math.BigInteger.valueOf(1));
            java.math.BigInteger nz_1 = new java.math.BigInteger(String.valueOf(zs_1.length)).subtract(java.math.BigInteger.valueOf(1));
            java.util.Map<java.math.BigInteger,java.math.BigInteger> xIndex_1 = ((java.util.Map<java.math.BigInteger,java.math.BigInteger>)(new java.util.LinkedHashMap<java.math.BigInteger, java.math.BigInteger>()));
            i_10 = java.math.BigInteger.valueOf(0);
            while (i_10.compareTo(new java.math.BigInteger(String.valueOf(xs_1.length))) < 0) {
xIndex_1.put(xs_1[_idx((xs_1).length, ((java.math.BigInteger)(i_10)).longValue())], i_10);
                i_10 = i_10.add(java.math.BigInteger.valueOf(1));
            }
            java.math.BigInteger[] dx_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            i_10 = java.math.BigInteger.valueOf(0);
            while (i_10.compareTo(nx_1) < 0) {
                dx_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dx_1), java.util.stream.Stream.of(xs_1[_idx((xs_1).length, ((java.math.BigInteger)(i_10.add(java.math.BigInteger.valueOf(1)))).longValue())].subtract(xs_1[_idx((xs_1).length, ((java.math.BigInteger)(i_10)).longValue())]))).toArray(java.math.BigInteger[]::new)));
                i_10 = i_10.add(java.math.BigInteger.valueOf(1));
            }
            java.math.BigInteger[] dy_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            i_10 = java.math.BigInteger.valueOf(0);
            while (i_10.compareTo(ny_1) < 0) {
                dy_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dy_1), java.util.stream.Stream.of(ys_1[_idx((ys_1).length, ((java.math.BigInteger)(i_10.add(java.math.BigInteger.valueOf(1)))).longValue())].subtract(ys_1[_idx((ys_1).length, ((java.math.BigInteger)(i_10)).longValue())]))).toArray(java.math.BigInteger[]::new)));
                i_10 = i_10.add(java.math.BigInteger.valueOf(1));
            }
            java.math.BigInteger[] dz_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            i_10 = java.math.BigInteger.valueOf(0);
            while (i_10.compareTo(nz_1) < 0) {
                dz_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dz_1), java.util.stream.Stream.of(zs_1[_idx((zs_1).length, ((java.math.BigInteger)(i_10.add(java.math.BigInteger.valueOf(1)))).longValue())].subtract(zs_1[_idx((zs_1).length, ((java.math.BigInteger)(i_10)).longValue())]))).toArray(java.math.BigInteger[]::new)));
                i_10 = i_10.add(java.math.BigInteger.valueOf(1));
            }
            boolean[][][] blockX_1 = ((boolean[][][])(make3DBool(new java.math.BigInteger(String.valueOf(xs_1.length)), ny_1, nz_1)));
            i_10 = java.math.BigInteger.valueOf(0);
            while (i_10.compareTo(new java.math.BigInteger(String.valueOf(faceXCoord_1.length))) < 0) {
                java.math.BigInteger coord_1 = faceXCoord_1[_idx((faceXCoord_1).length, ((java.math.BigInteger)(i_10)).longValue())];
                java.math.BigInteger[] polyY_1 = ((java.math.BigInteger[])(faceYPoly_1[_idx((faceYPoly_1).length, ((java.math.BigInteger)(i_10)).longValue())]));
                java.math.BigInteger[] polyZ_1 = ((java.math.BigInteger[])(faceZPoly_1[_idx((faceZPoly_1).length, ((java.math.BigInteger)(i_10)).longValue())]));
                java.math.BigInteger xi_3 = ((java.math.BigInteger)(xIndex_1).get(coord_1));
                java.math.BigInteger j_9 = java.math.BigInteger.valueOf(0);
                while (j_9.compareTo(ny_1) < 0) {
                    double cy_1 = (double)((double)((((Number)((ys_1[_idx((ys_1).length, ((java.math.BigInteger)(j_9)).longValue())].add(ys_1[_idx((ys_1).length, ((java.math.BigInteger)(j_9.add(java.math.BigInteger.valueOf(1)))).longValue())])))).doubleValue())) / (double)(2.0));
                    java.math.BigInteger k_3 = java.math.BigInteger.valueOf(0);
                    while (k_3.compareTo(nz_1) < 0) {
                        double cz_1 = (double)((double)((((Number)((zs_1[_idx((zs_1).length, ((java.math.BigInteger)(k_3)).longValue())].add(zs_1[_idx((zs_1).length, ((java.math.BigInteger)(k_3.add(java.math.BigInteger.valueOf(1)))).longValue())])))).doubleValue())) / (double)(2.0));
                        if (pointInPoly(((java.math.BigInteger[])(polyY_1)), ((java.math.BigInteger[])(polyZ_1)), (double)(cy_1), (double)(cz_1))) {
blockX_1[_idx((blockX_1).length, ((java.math.BigInteger)(xi_3)).longValue())][_idx((blockX_1[_idx((blockX_1).length, ((java.math.BigInteger)(xi_3)).longValue())]).length, ((java.math.BigInteger)(j_9)).longValue())][(int)(((java.math.BigInteger)(k_3)).longValue())] = true;
                        }
                        k_3 = k_3.add(java.math.BigInteger.valueOf(1));
                    }
                    j_9 = j_9.add(java.math.BigInteger.valueOf(1));
                }
                i_10 = i_10.add(java.math.BigInteger.valueOf(1));
            }
            boolean[][][] solid_1 = ((boolean[][][])(make3DBool(nx_1, ny_1, nz_1)));
            java.math.BigInteger j2_1 = java.math.BigInteger.valueOf(0);
            while (j2_1.compareTo(ny_1) < 0) {
                java.math.BigInteger k2_1 = java.math.BigInteger.valueOf(0);
                while (k2_1.compareTo(nz_1) < 0) {
                    boolean inside_2 = false;
                    java.math.BigInteger i2_1 = java.math.BigInteger.valueOf(0);
                    while (i2_1.compareTo(nx_1) < 0) {
                        if (blockX_1[_idx((blockX_1).length, ((java.math.BigInteger)(i2_1)).longValue())][_idx((blockX_1[_idx((blockX_1).length, ((java.math.BigInteger)(i2_1)).longValue())]).length, ((java.math.BigInteger)(j2_1)).longValue())][_idx((blockX_1[_idx((blockX_1).length, ((java.math.BigInteger)(i2_1)).longValue())][_idx((blockX_1[_idx((blockX_1).length, ((java.math.BigInteger)(i2_1)).longValue())]).length, ((java.math.BigInteger)(j2_1)).longValue())]).length, ((java.math.BigInteger)(k2_1)).longValue())]) {
                            inside_2 = !inside_2;
                        }
                        if (inside_2) {
solid_1[_idx((solid_1).length, ((java.math.BigInteger)(i2_1)).longValue())][_idx((solid_1[_idx((solid_1).length, ((java.math.BigInteger)(i2_1)).longValue())]).length, ((java.math.BigInteger)(j2_1)).longValue())][(int)(((java.math.BigInteger)(k2_1)).longValue())] = true;
                        }
                        i2_1 = i2_1.add(java.math.BigInteger.valueOf(1));
                    }
                    k2_1 = k2_1.add(java.math.BigInteger.valueOf(1));
                }
                j2_1 = j2_1.add(java.math.BigInteger.valueOf(1));
            }
            java.math.BigInteger volume_1 = java.math.BigInteger.valueOf(0);
            java.math.BigInteger i3_1 = java.math.BigInteger.valueOf(0);
            while (i3_1.compareTo(nx_1) < 0) {
                java.math.BigInteger j3_1 = java.math.BigInteger.valueOf(0);
                while (j3_1.compareTo(ny_1) < 0) {
                    java.math.BigInteger k3_1 = java.math.BigInteger.valueOf(0);
                    while (k3_1.compareTo(nz_1) < 0) {
                        if (solid_1[_idx((solid_1).length, ((java.math.BigInteger)(i3_1)).longValue())][_idx((solid_1[_idx((solid_1).length, ((java.math.BigInteger)(i3_1)).longValue())]).length, ((java.math.BigInteger)(j3_1)).longValue())][_idx((solid_1[_idx((solid_1).length, ((java.math.BigInteger)(i3_1)).longValue())][_idx((solid_1[_idx((solid_1).length, ((java.math.BigInteger)(i3_1)).longValue())]).length, ((java.math.BigInteger)(j3_1)).longValue())]).length, ((java.math.BigInteger)(k3_1)).longValue())]) {
                            volume_1 = volume_1.add(dx_1[_idx((dx_1).length, ((java.math.BigInteger)(i3_1)).longValue())].multiply(dy_1[_idx((dy_1).length, ((java.math.BigInteger)(j3_1)).longValue())]).multiply(dz_1[_idx((dz_1).length, ((java.math.BigInteger)(k3_1)).longValue())]));
                        }
                        k3_1 = k3_1.add(java.math.BigInteger.valueOf(1));
                    }
                    j3_1 = j3_1.add(java.math.BigInteger.valueOf(1));
                }
                i3_1 = i3_1.add(java.math.BigInteger.valueOf(1));
            }
            System.out.println("The bulk is composed of " + _p(volume_1) + " units.");
            case_1 = case_1.add(java.math.BigInteger.valueOf(1));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
