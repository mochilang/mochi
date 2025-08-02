public class Main {
    static class Point {
        double x;
        double y;
        double z;
        Point(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z));
        }
    }

    static class Edge {
        int pn1;
        int pn2;
        int fn1;
        int fn2;
        Point cp;
        Edge(int pn1, int pn2, int fn1, int fn2, Point cp) {
            this.pn1 = pn1;
            this.pn2 = pn2;
            this.fn1 = fn1;
            this.fn2 = fn2;
            this.cp = cp;
        }
        @Override public String toString() {
            return String.format("{'pn1': %s, 'pn2': %s, 'fn1': %s, 'fn2': %s, 'cp': %s}", String.valueOf(pn1), String.valueOf(pn2), String.valueOf(fn1), String.valueOf(fn2), String.valueOf(cp));
        }
    }

    static class PointEx {
        Point p;
        int n;
        PointEx(Point p, int n) {
            this.p = p;
            this.n = n;
        }
        @Override public String toString() {
            return String.format("{'p': %s, 'n': %s}", String.valueOf(p), String.valueOf(n));
        }
    }


    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String fmt4(double x) {
        double y = x * 10000.0;
        if (y >= 0) {
            y = y + 0.5;
        } else {
            y = y - 0.5;
        }
        y = ((Number)((((Number)(y)).intValue()))).doubleValue() / 10000.0;
        String s = _p(y);
        int dot = ((Number)(s.indexOf("."))).intValue();
        if (dot == 0 - 1) {
            s = s + ".0000";
        } else {
            int decs = _runeLen(s) - dot - 1;
            if (decs > 4) {
                s = _substr(s, 0, dot + 5);
            } else {
                while (decs < 4) {
                    s = s + "0";
                    decs = decs + 1;
                }
            }
        }
        if (x >= 0.0) {
            s = " " + s;
        }
        return s;
    }

    static String fmt2(int n) {
        String s_1 = _p(n);
        if (_runeLen(s_1) < 2) {
            return " " + s_1;
        }
        return s_1;
    }

    static Point sumPoint(Point p1, Point p2) {
        return new Point(p1.x + p2.x, p1.y + p2.y, p1.z + p2.z);
    }

    static Point mulPoint(Point p, double m) {
        return new Point(p.x * m, p.y * m, p.z * m);
    }

    static Point divPoint(Point p, double d) {
        return mulPoint(p, 1.0 / d);
    }

    static Point centerPoint(Point p1, Point p2) {
        return divPoint(sumPoint(p1, p2), 2.0);
    }

    static Point[] getFacePoints(Point[] points, int[][] faces) {
        Point[] facePoints = ((Point[])(new Point[]{}));
        int i_1 = 0;
        while (i_1 < faces.length) {
            int[] face = ((int[])(faces[i_1]));
            Point fp = new Point(0.0, 0.0, 0.0);
            for (int idx : face) {
                fp = sumPoint(fp, points[idx]);
            }
            fp = divPoint(fp, (((Number)(face.length)).doubleValue()));
            facePoints = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(facePoints), java.util.stream.Stream.of(fp)).toArray(Point[]::new)));
            i_1 = i_1 + 1;
        }
        return facePoints;
    }

    static int[][] sortEdges(int[][] edges) {
        int[][] res = ((int[][])(new int[][]{}));
        int[][] tmp = ((int[][])(edges));
        while (tmp.length > 0) {
            int[] min = ((int[])(tmp[0]));
            int idx = 0;
            int j = 1;
            while (j < tmp.length) {
                int[] e = ((int[])(tmp[j]));
                if (e[0] < min[0] || (e[0] == min[0] && (e[1] < min[1] || (e[1] == min[1] && e[2] < min[2])))) {
                    min = ((int[])(e));
                    idx = j;
                }
                j = j + 1;
            }
            res = ((int[][])(appendObj(res, min)));
            int[][] out = ((int[][])(new int[][]{}));
            int k = 0;
            while (k < tmp.length) {
                if (k != idx) {
                    out = ((int[][])(appendObj(out, tmp[k])));
                }
                k = k + 1;
            }
            tmp = ((int[][])(out));
        }
        return res;
    }

    static Edge[] getEdgesFaces(Point[] points, int[][] faces) {
        int[][] edges = ((int[][])(new int[][]{}));
        int fnum = 0;
        while (fnum < faces.length) {
            int[] face_1 = ((int[])(faces[fnum]));
            int numP = face_1.length;
            int pi = 0;
            while (pi < numP) {
                int pn1 = face_1[pi];
                int pn2 = 0;
                if (pi < numP - 1) {
                    pn2 = face_1[pi + 1];
                } else {
                    pn2 = face_1[0];
                }
                if (pn1 > pn2) {
                    int tmpn = pn1;
                    pn1 = pn2;
                    pn2 = tmpn;
                }
                edges = ((int[][])(appendObj(edges, new int[]{pn1, pn2, fnum})));
                pi = pi + 1;
            }
            fnum = fnum + 1;
        }
        edges = ((int[][])(sortEdges(((int[][])(edges)))));
        int[][] merged = ((int[][])(new int[][]{}));
        int idx_1 = 0;
        while (idx_1 < edges.length) {
            int[] e1 = ((int[])(edges[idx_1]));
            if (idx_1 < edges.length - 1) {
                int[] e2 = ((int[])(edges[idx_1 + 1]));
                if (e1[0] == e2[0] && e1[1] == e2[1]) {
                    merged = ((int[][])(appendObj(merged, new int[]{e1[0], e1[1], e1[2], e2[2]})));
                    idx_1 = idx_1 + 2;
                    continue;
                }
            }
            merged = ((int[][])(appendObj(merged, new int[]{e1[0], e1[1], e1[2], -1})));
            idx_1 = idx_1 + 1;
        }
        Edge[] edgesCenters = ((Edge[])(new Edge[]{}));
        for (int[] me : merged) {
            Point p1 = points[me[0]];
            Point p2 = points[me[1]];
            Point cp = centerPoint(p1, p2);
            edgesCenters = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(edgesCenters), java.util.stream.Stream.of(new Edge(me[0], me[1], me[2], me[3], cp))).toArray(Edge[]::new)));
        }
        return edgesCenters;
    }

    static Point[] getEdgePoints(Point[] points, Edge[] edgesFaces, Point[] facePoints) {
        Point[] edgePoints = ((Point[])(new Point[]{}));
        int i_2 = 0;
        while (i_2 < edgesFaces.length) {
            Edge edge = edgesFaces[i_2];
            Point cp_1 = edge.cp;
            Point fp1 = facePoints[edge.fn1];
            Point fp2 = fp1;
            if (edge.fn2 != 0 - 1) {
                fp2 = facePoints[edge.fn2];
            }
            Point cfp = centerPoint(fp1, fp2);
            edgePoints = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(edgePoints), java.util.stream.Stream.of(centerPoint(cp_1, cfp))).toArray(Point[]::new)));
            i_2 = i_2 + 1;
        }
        return edgePoints;
    }

    static Point[] getAvgFacePoints(Point[] points, int[][] faces, Point[] facePoints) {
        int numP_1 = points.length;
        PointEx[] temp = ((PointEx[])(new PointEx[]{}));
        int i_3 = 0;
        while (i_3 < numP_1) {
            temp = ((PointEx[])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp), java.util.stream.Stream.of(new PointEx(new Point(0.0, 0.0, 0.0), 0))).toArray(PointEx[]::new)));
            i_3 = i_3 + 1;
        }
        int fnum_1 = 0;
        while (fnum_1 < faces.length) {
            Point fp_1 = facePoints[fnum_1];
            for (int pn : faces[fnum_1]) {
                PointEx tp = temp[pn];
temp[pn] = new PointEx(sumPoint(tp.p, fp_1), tp.n + 1);
            }
            fnum_1 = fnum_1 + 1;
        }
        Point[] avg = ((Point[])(new Point[]{}));
        int j_1 = 0;
        while (j_1 < numP_1) {
            PointEx tp_1 = temp[j_1];
            avg = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(avg), java.util.stream.Stream.of(divPoint(tp_1.p, ((Number)(tp_1.n)).doubleValue()))).toArray(Point[]::new)));
            j_1 = j_1 + 1;
        }
        return avg;
    }

    static Point[] getAvgMidEdges(Point[] points, Edge[] edgesFaces) {
        int numP_2 = points.length;
        PointEx[] temp_1 = ((PointEx[])(new PointEx[]{}));
        int i_4 = 0;
        while (i_4 < numP_2) {
            temp_1 = ((PointEx[])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp_1), java.util.stream.Stream.of(new PointEx(new Point(0.0, 0.0, 0.0), 0))).toArray(PointEx[]::new)));
            i_4 = i_4 + 1;
        }
        for (Edge edge : edgesFaces) {
            Point cp_2 = edge.cp;
            int[] arr = ((int[])(new int[]{edge.pn1, edge.pn2}));
            for (int pn : arr) {
                PointEx tp_2 = temp_1[pn];
temp_1[pn] = new PointEx(sumPoint(tp_2.p, cp_2), tp_2.n + 1);
            }
        }
        Point[] avg_1 = ((Point[])(new Point[]{}));
        int j_2 = 0;
        while (j_2 < numP_2) {
            PointEx tp_3 = temp_1[j_2];
            avg_1 = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(avg_1), java.util.stream.Stream.of(divPoint(tp_3.p, ((Number)(tp_3.n)).doubleValue()))).toArray(Point[]::new)));
            j_2 = j_2 + 1;
        }
        return avg_1;
    }

    static int[] getPointsFaces(Point[] points, int[][] faces) {
        int[] pf = ((int[])(new int[]{}));
        int i_5 = 0;
        while (i_5 < points.length) {
            pf = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pf), java.util.stream.IntStream.of(0)).toArray()));
            i_5 = i_5 + 1;
        }
        int fnum_2 = 0;
        while (fnum_2 < faces.length) {
            for (int pn : faces[fnum_2]) {
pf[pn] = pf[pn] + 1;
            }
            fnum_2 = fnum_2 + 1;
        }
        return pf;
    }

    static Point[] getNewPoints(Point[] points, int[] pf, Point[] afp, Point[] ame) {
        Point[] newPts = ((Point[])(new Point[]{}));
        int i_6 = 0;
        while (i_6 < points.length) {
            double n = ((double)(pf[i_6]));
            double m1 = (n - 3.0) / n;
            double m2 = 1.0 / n;
            double m3 = 2.0 / n;
            Point old = points[i_6];
            Point p1_1 = mulPoint(old, m1);
            Point p2_1 = mulPoint(afp[i_6], m2);
            Point p3 = mulPoint(ame[i_6], m3);
            newPts = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(newPts), java.util.stream.Stream.of(sumPoint(sumPoint(p1_1, p2_1), p3))).toArray(Point[]::new)));
            i_6 = i_6 + 1;
        }
        return newPts;
    }

    static String key(int a, int b) {
        if (a < b) {
            return _p(a) + "," + _p(b);
        }
        return _p(b) + "," + _p(a);
    }

    static Object[] cmcSubdiv(Point[] points, int[][] faces) {
        Point[] facePoints_1 = ((Point[])(getFacePoints(((Point[])(points)), ((int[][])(faces)))));
        Edge[] edgesFaces = ((Edge[])(getEdgesFaces(((Point[])(points)), ((int[][])(faces)))));
        Point[] edgePoints_1 = ((Point[])(getEdgePoints(((Point[])(points)), ((Edge[])(edgesFaces)), ((Point[])(facePoints_1)))));
        Point[] avgFacePoints = ((Point[])(getAvgFacePoints(((Point[])(points)), ((int[][])(faces)), ((Point[])(facePoints_1)))));
        Point[] avgMidEdges = ((Point[])(getAvgMidEdges(((Point[])(points)), ((Edge[])(edgesFaces)))));
        int[] pointsFaces = ((int[])(getPointsFaces(((Point[])(points)), ((int[][])(faces)))));
        Point[] newPoints = ((Point[])(getNewPoints(((Point[])(points)), ((int[])(pointsFaces)), ((Point[])(avgFacePoints)), ((Point[])(avgMidEdges)))));
        int[] facePointNums = ((int[])(new int[]{}));
        int nextPoint = newPoints.length;
        for (Point fp : facePoints_1) {
            newPoints = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(newPoints), java.util.stream.Stream.of(fp)).toArray(Point[]::new)));
            facePointNums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(facePointNums), java.util.stream.IntStream.of(nextPoint)).toArray()));
            nextPoint = nextPoint + 1;
        }
        java.util.Map<String,Integer> edgePointNums = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int idx_2 = 0;
        while (idx_2 < edgesFaces.length) {
            Edge e_1 = edgesFaces[idx_2];
            newPoints = ((Point[])(java.util.stream.Stream.concat(java.util.Arrays.stream(newPoints), java.util.stream.Stream.of(edgePoints_1[idx_2])).toArray(Point[]::new)));
edgePointNums.put(key(e_1.pn1, e_1.pn2), nextPoint);
            nextPoint = nextPoint + 1;
            idx_2 = idx_2 + 1;
        }
        int[][] newFaces = ((int[][])(new int[][]{}));
        int fnum_3 = 0;
        while (fnum_3 < faces.length) {
            int[] oldFace = ((int[])(faces[fnum_3]));
            if (oldFace.length == 4) {
                int a = oldFace[0];
                int b = oldFace[1];
                int c = oldFace[2];
                int d = oldFace[3];
                int fpnum = facePointNums[fnum_3];
                int ab = (int)(((int)(edgePointNums).getOrDefault(key(a, b), 0)));
                int da = (int)(((int)(edgePointNums).getOrDefault(key(d, a), 0)));
                int bc = (int)(((int)(edgePointNums).getOrDefault(key(b, c), 0)));
                int cd = (int)(((int)(edgePointNums).getOrDefault(key(c, d), 0)));
                newFaces = ((int[][])(appendObj(newFaces, new int[]{a, ab, fpnum, da})));
                newFaces = ((int[][])(appendObj(newFaces, new int[]{b, bc, fpnum, ab})));
                newFaces = ((int[][])(appendObj(newFaces, new int[]{c, cd, fpnum, bc})));
                newFaces = ((int[][])(appendObj(newFaces, new int[]{d, da, fpnum, cd})));
            }
            fnum_3 = fnum_3 + 1;
        }
        return new Object[]{newPoints, newFaces};
    }

    static String formatPoint(Point p) {
        return "[" + String.valueOf(fmt4(p.x)) + " " + String.valueOf(fmt4(p.y)) + " " + String.valueOf(fmt4(p.z)) + "]";
    }

    static String formatFace(int[] f) {
        if (f.length == 0) {
            return "[]";
        }
        String s_2 = "[" + String.valueOf(fmt2(f[0]));
        int i_7 = 1;
        while (i_7 < f.length) {
            s_2 = s_2 + " " + String.valueOf(fmt2(f[i_7]));
            i_7 = i_7 + 1;
        }
        s_2 = s_2 + "]";
        return s_2;
    }

    static void main() {
        Point[] inputPoints = ((Point[])(new Point[]{new Point(-1.0, 1.0, 1.0), new Point(-1.0, -1.0, 1.0), new Point(1.0, -1.0, 1.0), new Point(1.0, 1.0, 1.0), new Point(1.0, -1.0, -1.0), new Point(1.0, 1.0, -1.0), new Point(-1.0, -1.0, -1.0), new Point(-1.0, 1.0, -1.0)}));
        int[][] inputFaces = ((int[][])(new int[][]{new int[]{0, 1, 2, 3}, new int[]{3, 2, 4, 5}, new int[]{5, 4, 6, 7}, new int[]{7, 0, 3, 5}, new int[]{7, 6, 1, 0}, new int[]{6, 1, 2, 4}}));
        Point[] outputPoints = ((Point[])(inputPoints));
        int[][] outputFaces = ((int[][])(inputFaces));
        int i_8 = 0;
        while (i_8 < 1) {
            Object[] res_1 = ((Object[])(cmcSubdiv(((Point[])(outputPoints)), ((int[][])(outputFaces)))));
            outputPoints = ((Point[])(res_1[0]));
            outputFaces = ((int[][])(res_1[1]));
            i_8 = i_8 + 1;
        }
        for (Point p : outputPoints) {
            System.out.println(formatPoint(p));
        }
        System.out.println("");
        for (int[] f : outputFaces) {
            System.out.println(formatFace(((int[])(f))));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
