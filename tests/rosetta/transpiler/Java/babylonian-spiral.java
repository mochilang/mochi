public class Main {

    static java.util.Map<String,Integer>[] push(java.util.Map<String,Integer>[] h, java.util.Map<String,Integer> it) {
        h = appendObj(h, it);
        int i = h.length - 1;
        while (i > 0 && (boolean)(((Object)h[i - 1].get("s"))) > ((Number)(((Object)h[i].get("s")))).intValue()) {
            java.util.Map<String,Integer> tmp = h[i - 1];
h[i - 1] = h[i];
h[i] = tmp;
            i = i - 1;
        }
        return h;
    }

    static java.util.Map<String,Object> step(java.util.Map<String,Integer>[] h, int nv, int[] dir) {
        while (h.length == 0 || nv * nv <= ((Number)(((Object)h[0].get("s")))).intValue()) {
            h = push(h, new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("s", nv * nv), java.util.Map.entry("a", nv), java.util.Map.entry("b", 0))));
            nv = nv + 1;
        }
        Object s = (Object)(((Object)h[0].get("s")));
        int[][] v = new int[][]{};
        while ((h.length > 0 && (boolean)(((Object)h[0].get("s"))).equals(s))) {
            java.util.Map<String,Integer> it = h[0];
            h = java.util.Arrays.copyOfRange(h, 1, h.length);
            v = appendObj(v, new int[]{((int)it.getOrDefault("a", 0)), ((int)it.getOrDefault("b", 0))});
            if ((int)(((int)it.getOrDefault("a", 0))) > (int)(((int)it.getOrDefault("b", 0)))) {
                h = push(h, new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("s", (int)(((int)it.getOrDefault("a", 0))) * (int)(((int)it.getOrDefault("a", 0))) + ((int)(((int)it.getOrDefault("b", 0))) + 1) * ((int)(((int)it.getOrDefault("b", 0))) + 1)), java.util.Map.entry("a", ((int)it.getOrDefault("a", 0))), java.util.Map.entry("b", (int)(((int)it.getOrDefault("b", 0))) + 1))));
            }
        }
        int[][] list = new int[][]{};
        for (int[] p : v) {
            list = appendObj(list, p);
        }
        int[][] temp = list;
        for (int[] p : temp) {
            if (p[0] != p[1]) {
                list = appendObj(list, new int[]{p[1], p[0]});
            }
        }
        temp = list;
        for (int[] p : temp) {
            if (p[1] != 0) {
                list = appendObj(list, new int[]{p[0], -p[1]});
            }
        }
        temp = list;
        for (int[] p : temp) {
            if (p[0] != 0) {
                list = appendObj(list, new int[]{-p[0], p[1]});
            }
        }
        int bestDot = -999999999;
        int[] best = dir;
        for (int[] p : list) {
            int cross = p[0] * dir[1] - p[1] * dir[0];
            if (cross >= 0) {
                int dot = p[0] * dir[0] + p[1] * dir[1];
                if (dot > bestDot) {
                    bestDot = dot;
                    best = p;
                }
            }
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("d", best), java.util.Map.entry("heap", h), java.util.Map.entry("n", nv)));
    }

    static int[][] positions(int n) {
        int[][] pos = new int[][]{};
        int x = 0;
        int y = 0;
        int[] dir = new int[]{0, 1};
        java.util.Map<String,Integer>[] heap = new java.util.Map<String,Integer>[]{};
        int nv = 1;
        int i = 0;
        while (i < n) {
            pos = appendObj(pos, new int[]{x, y});
            java.util.Map<String,Object> st = step(heap, nv, dir);
            dir = (int[])(((int[])st.get("d")));
            heap = (java.util.Map<String,Integer>[])(((java.util.Map<String,Integer>[])st.get("heap")));
            nv = (int)(((int)st.getOrDefault("n", 0)));
            x = x + dir[0];
            y = y + dir[1];
            i = i + 1;
        }
        return pos;
    }

    static String pad(String s, int w) {
        String r = s;
        while (r.length() < w) {
            r = String.valueOf(r + " ");
        }
        return r;
    }

    static void main() {
        int[][] pts = positions(40);
        System.out.println("The first 40 Babylonian spiral points are:");
        String line = "";
        int i = 0;
        while (i < pts.length) {
            int[] p = pts[i];
            String s = String.valueOf(pad(String.valueOf(String.valueOf(String.valueOf(String.valueOf("(" + String.valueOf(p[0])) + ", ") + String.valueOf(p[1])) + ")"), 10));
            line = String.valueOf(line + s);
            if (((Number)(Math.floorMod((i + 1), 10))).intValue() == 0) {
                System.out.println(line);
                line = "";
            }
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
