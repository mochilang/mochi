public class Main {
    static class Edge {
        int a;
        int b;
        Edge(int a, int b) {
            this.a = a;
            this.b = b;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'b': %s}", String.valueOf(a), String.valueOf(b));
        }
    }


    static boolean contains(int[] xs, int v) {
        for (int x : xs) {
            if (x == v) {
                return true;
            }
        }
        return false;
    }

    static int[] copyInts(int[] xs) {
        int[] out = new int[]{};
        for (int x : xs) {
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(x)).toArray();
        }
        return out;
    }

    static boolean sliceEqual(int[] a, int[] b) {
        int i = 0;
        while (i < a.length) {
            if (a[i] != b[i]) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static void reverse(int[] xs) {
        int i_1 = 0;
        int j = xs.length - 1;
        while (i_1 < j) {
            int t = xs[i_1];
xs[i_1] = xs[j];
xs[j] = t;
            i_1 = i_1 + 1;
            j = j - 1;
        }
    }

    static boolean perimEqual(int[] p1, int[] p2) {
        if (p1.length != p2.length) {
            return false;
        }
        for (int v : p1) {
            if (!(Boolean)contains(p2, v)) {
                return false;
            }
        }
        int[] c = copyInts(p1);
        int r = 0;
        while (r < 2) {
            int i_2 = 0;
            while (i_2 < c.length) {
                if (((Boolean)(sliceEqual(c, p2)))) {
                    return true;
                }
                int t_1 = c[c.length - 1];
                int j_1 = c.length - 1;
                while (j_1 > 0) {
c[j_1] = c[j_1 - 1];
                    j_1 = j_1 - 1;
                }
c[0] = t_1;
                i_2 = i_2 + 1;
            }
            reverse(c);
            r = r + 1;
        }
        return false;
    }

    static Edge[] sortEdges(Edge[] es) {
        Edge[] arr = es;
        int n = arr.length;
        int i_3 = 0;
        while (i_3 < n) {
            int j_2 = 0;
            while (j_2 < n - 1) {
                Edge a = arr[j_2];
                Edge b = arr[j_2 + 1];
                if (a.a > b.a || (a.a == b.a && a.b > b.b)) {
arr[j_2] = b;
arr[j_2 + 1] = a;
                }
                j_2 = j_2 + 1;
            }
            i_3 = i_3 + 1;
        }
        return arr;
    }

    static Edge[] concat(Edge[] a, Edge[] b) {
        Edge[] out_1 = new Edge[]{};
        for (Edge x : a) {
            out_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(out_1), java.util.stream.Stream.of(x)).toArray(Edge[]::new);
        }
        for (Edge x : b) {
            out_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(out_1), java.util.stream.Stream.of(x)).toArray(Edge[]::new);
        }
        return out_1;
    }

    static Object faceToPerim(Edge[] face) {
        int le = face.length;
        if (le == 0) {
            return null;
        }
        Edge[] edges = new Edge[]{};
        int i_4 = 0;
        while (i_4 < le) {
            Edge e = face[i_4];
            if (e.b <= e.a) {
                return null;
            }
            edges = java.util.stream.Stream.concat(java.util.Arrays.stream(edges), java.util.stream.Stream.of(e)).toArray(Edge[]::new);
            i_4 = i_4 + 1;
        }
        edges = sortEdges(edges);
        Edge firstEdge = edges[0];
        int[] perim = new int[]{firstEdge.a, firstEdge.b};
        int first = firstEdge.a;
        int last = firstEdge.b;
        edges = java.util.Arrays.copyOfRange(edges, 1, edges.length);
        le = edges.length;
        boolean done = false;
        while (le > 0 && (!done)) {
            int idx = 0;
            boolean found = false;
            while (idx < le) {
                Edge e_1 = edges[idx];
                if (e_1.a == last) {
                    perim = java.util.stream.IntStream.concat(java.util.Arrays.stream(perim), java.util.stream.IntStream.of(e_1.b)).toArray();
                    last = e_1.b;
                    found = true;
                } else                 if (e_1.b == last) {
                    perim = java.util.stream.IntStream.concat(java.util.Arrays.stream(perim), java.util.stream.IntStream.of(e_1.a)).toArray();
                    last = e_1.a;
                    found = true;
                }
                if (found) {
                    edges = concat(java.util.Arrays.copyOfRange(edges, 0, idx), java.util.Arrays.copyOfRange(edges, idx + 1, edges.length));
                    le = le - 1;
                    if (last == first) {
                        if (le == 0) {
                            done = true;
                        } else {
                            return null;
                        }
                    }
                    break;
                }
                idx = idx + 1;
            }
            if (!found) {
                return null;
            }
        }
        return java.util.Arrays.copyOfRange(perim, 0, perim.length - 1);
    }

    static String listStr(int[] xs) {
        String s = "[";
        int i_5 = 0;
        while (i_5 < xs.length) {
            s = s + (String)(_p(_geti(xs, i_5)));
            if (i_5 < xs.length - 1) {
                s = s + " ";
            }
            i_5 = i_5 + 1;
        }
        s = s + "]";
        return s;
    }

    static void main() {
        System.out.println("Perimeter format equality checks:");
        System.out.println("  Q == R is " + (String)(_p(perimEqual(new int[]{8, 1, 3}, new int[]{1, 3, 8}))));
        System.out.println("  U == V is " + (String)(_p(perimEqual(new int[]{18, 8, 14, 10, 12, 17, 19}, new int[]{8, 14, 10, 12, 17, 19, 18}))));
        Edge[] e_2 = new Edge[]{new Edge(7, 11), new Edge(1, 11), new Edge(1, 7)};
        Edge[] f = new Edge[]{new Edge(11, 23), new Edge(1, 17), new Edge(17, 23), new Edge(1, 11)};
        Edge[] g = new Edge[]{new Edge(8, 14), new Edge(17, 19), new Edge(10, 12), new Edge(10, 14), new Edge(12, 17), new Edge(8, 18), new Edge(18, 19)};
        Edge[] h = new Edge[]{new Edge(1, 3), new Edge(9, 11), new Edge(3, 11), new Edge(1, 11)};
        System.out.println("\nEdge to perimeter format translations:");
        Edge[][] faces = new Edge[][]{e_2, f, g, h};
        String[] names = new String[]{"E", "F", "G", "H"};
        int idx_1 = 0;
        while (idx_1 < faces.length) {
            Object per = faceToPerim(faces[idx_1]);
            if ((per == null)) {
                System.out.println("  " + names[idx_1] + " => Invalid edge format");
            } else {
                System.out.println("  " + names[idx_1] + " => " + String.valueOf(listStr(((int[])(per)))));
            }
            idx_1 = idx_1 + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
