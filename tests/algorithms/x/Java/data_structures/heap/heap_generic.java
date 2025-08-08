public class Main {
    static class Heap {
        int[][] arr;
        java.util.Map<Integer,Integer> pos_map;
        int size;
        java.util.function.Function<Integer,Integer> key;
        Heap(int[][] arr, java.util.Map<Integer,Integer> pos_map, int size, java.util.function.Function<Integer,Integer> key) {
            this.arr = arr;
            this.pos_map = pos_map;
            this.size = size;
            this.key = key;
        }
        Heap() {}
        @Override public String toString() {
            return String.format("{'arr': %s, 'pos_map': %s, 'size': %s, 'key': %s}", String.valueOf(arr), String.valueOf(pos_map), String.valueOf(size), String.valueOf(key));
        }
    }

    static Heap h = null;

    static Heap new_heap(java.util.function.Function<Integer,Integer> key) {
        return new Heap(new int[][]{}, new java.util.LinkedHashMap<Integer, Integer>(), 0, key);
    }

    static int parent(int i) {
        if (i > 0) {
            return ((int)(Math.floorDiv((i - 1), 2)));
        }
        return -1;
    }

    static int left(int i, int size) {
        int l = 2 * i + 1;
        if (l < size) {
            return l;
        }
        return -1;
    }

    static int right(int i, int size) {
        int r = 2 * i + 2;
        if (r < size) {
            return r;
        }
        return -1;
    }

    static void swap(Heap h, int i, int j) {
        int[][] arr = ((int[][])(h.arr));
        int item_i = arr[i][0];
        int item_j = arr[j][0];
        java.util.Map<Integer,Integer> pm = h.pos_map;
pm.put(item_i, j + 1);
pm.put(item_j, i + 1);
h.pos_map = pm;
        int[] tmp = ((int[])(arr[i]));
arr[i] = ((int[])(arr[j]));
arr[j] = ((int[])(tmp));
h.arr = arr;
    }

    static boolean cmp(Heap h, int i, int j) {
        int[][] arr_1 = ((int[][])(h.arr));
        return arr_1[i][1] < arr_1[j][1];
    }

    static int get_valid_parent(Heap h, int i) {
        int vp = i;
        int l_1 = left(i, h.size);
        if (l_1 != 0 - 1 && ((Boolean)(cmp(h, l_1, vp))) == false) {
            vp = l_1;
        }
        int r_1 = right(i, h.size);
        if (r_1 != 0 - 1 && ((Boolean)(cmp(h, r_1, vp))) == false) {
            vp = r_1;
        }
        return vp;
    }

    static void heapify_up(Heap h, int index) {
        int idx = index;
        int p = parent(idx);
        while (p != 0 - 1 && ((Boolean)(cmp(h, idx, p))) == false) {
            swap(h, idx, p);
            idx = p;
            p = parent(p);
        }
    }

    static void heapify_down(Heap h, int index) {
        int idx_1 = index;
        int vp_1 = get_valid_parent(h, idx_1);
        while (vp_1 != idx_1) {
            swap(h, idx_1, vp_1);
            idx_1 = vp_1;
            vp_1 = get_valid_parent(h, idx_1);
        }
    }

    static void update_item(Heap h, int item, int item_value) {
        java.util.Map<Integer,Integer> pm_1 = h.pos_map;
        if ((int)(((int)(pm_1).getOrDefault(item, 0))) == 0) {
            return;
        }
        int index = (int)(((int)(pm_1).getOrDefault(item, 0))) - 1;
        int[][] arr_2 = ((int[][])(h.arr));
arr_2[index] = ((int[])(new Object[]{item, h.key.apply(item_value)}));
h.arr = arr_2;
h.pos_map = pm_1;
        heapify_up(h, index);
        heapify_down(h, index);
    }

    static void delete_item(Heap h, int item) {
        java.util.Map<Integer,Integer> pm_2 = h.pos_map;
        if ((int)(((int)(pm_2).getOrDefault(item, 0))) == 0) {
            return;
        }
        int index_1 = (int)(((int)(pm_2).getOrDefault(item, 0))) - 1;
pm_2.put(item, 0);
        int[][] arr_3 = ((int[][])(h.arr));
        int last_index = h.size - 1;
        if (index_1 != last_index) {
arr_3[index_1] = ((int[])(arr_3[last_index]));
            int moved = arr_3[index_1][0];
pm_2.put(moved, index_1 + 1);
        }
h.size = h.size - 1;
h.arr = arr_3;
h.pos_map = pm_2;
        if (h.size > index_1) {
            heapify_up(h, index_1);
            heapify_down(h, index_1);
        }
    }

    static void insert_item(Heap h, int item, int item_value) {
        Object arr_4 = h.arr;
        int arr_len = arr_4.length;
        if (arr_len == h.size) {
            arr_4 = appendObj((int[][])arr_4, new int[]{item, h.key.apply(item_value)});
        } else {
arr_4[h.size] = new Object[]{item, h.key.apply(item_value)};
        }
        java.util.Map<Integer,Integer> pm_3 = h.pos_map;
pm_3.put(item, h.size + 1);
h.size = h.size + 1;
h.arr = arr_4;
h.pos_map = pm_3;
        heapify_up(h, h.size - 1);
    }

    static int[] get_top(Heap h) {
        int[][] arr_5 = ((int[][])(h.arr));
        if (h.size > 0) {
            return arr_5[0];
        }
        return new int[]{};
    }

    static int[] extract_top(Heap h) {
        int[] top = ((int[])(get_top(h)));
        if (top.length > 0) {
            delete_item(h, top[0]);
        }
        return top;
    }

    static int identity(int x) {
        return x;
    }

    static int negate(int x) {
        return 0 - x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            h = new_heap(Main::identity);
            insert_item(h, 5, 34);
            insert_item(h, 6, 31);
            insert_item(h, 7, 37);
            System.out.println(_p(get_top(h)));
            System.out.println(_p(extract_top(h)));
            System.out.println(_p(extract_top(h)));
            System.out.println(_p(extract_top(h)));
            h = new_heap(Main::negate);
            insert_item(h, 5, 34);
            insert_item(h, 6, 31);
            insert_item(h, 7, 37);
            System.out.println(_p(get_top(h)));
            System.out.println(_p(extract_top(h)));
            System.out.println(_p(extract_top(h)));
            System.out.println(_p(extract_top(h)));
            insert_item(h, 8, 45);
            insert_item(h, 9, 40);
            insert_item(h, 10, 50);
            System.out.println(_p(get_top(h)));
            update_item(h, 10, 30);
            System.out.println(_p(get_top(h)));
            delete_item(h, 10);
            System.out.println(_p(get_top(h)));
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
}
