public class Main {
    static class Node {
        String name;
        int val;
        Node(String name, int val) {
            this.name = name;
            this.val = val;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'val': %s}", String.valueOf(name), String.valueOf(val));
        }
    }

    static class MinHeap {
        Node[] heap;
        java.util.Map<String,Integer> idx_of_element;
        java.util.Map<String,Integer> heap_dict;
        MinHeap(Node[] heap, java.util.Map<String,Integer> idx_of_element, java.util.Map<String,Integer> heap_dict) {
            this.heap = heap;
            this.idx_of_element = idx_of_element;
            this.heap_dict = heap_dict;
        }
        MinHeap() {}
        @Override public String toString() {
            return String.format("{'heap': %s, 'idx_of_element': %s, 'heap_dict': %s}", String.valueOf(heap), String.valueOf(idx_of_element), String.valueOf(heap_dict));
        }
    }

    static Node r = null;
    static Node b = null;
    static Node a = null;
    static Node x = null;
    static Node e = null;
    static MinHeap my_min_heap = null;

    static int get_parent_idx(int idx) {
        return ((int)(Math.floorDiv((idx - 1), 2)));
    }

    static int get_left_child_idx(int idx) {
        return idx * 2 + 1;
    }

    static int get_right_child_idx(int idx) {
        return idx * 2 + 2;
    }

    static java.util.Map<String,Integer> remove_key(java.util.Map<String,Integer> m, String k) {
        java.util.Map<String,Integer> out = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        for (String key : m.keySet()) {
            if (!(key.equals(k))) {
out.put(key, (int)(((int)(m).getOrDefault(key, 0))));
            }
        }
        return out;
    }

    static Node[] slice_without_last(Node[] xs) {
        Node[] res = ((Node[])(new Node[]{}));
        int i = 0;
        while (i < xs.length - 1) {
            res = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[i])).toArray(Node[]::new)));
            i = i + 1;
        }
        return res;
    }

    static void sift_down(MinHeap mh, int idx) {
        Node[] heap = ((Node[])(mh.heap));
        java.util.Map<String,Integer> idx_map = mh.idx_of_element;
        int i_1 = idx;
        while (true) {
            int left = get_left_child_idx(i_1);
            int right = get_right_child_idx(i_1);
            int smallest = i_1;
            if (left < heap.length && heap[left].val < heap[smallest].val) {
                smallest = left;
            }
            if (right < heap.length && heap[right].val < heap[smallest].val) {
                smallest = right;
            }
            if (smallest != i_1) {
                Node tmp = heap[i_1];
heap[i_1] = heap[smallest];
heap[smallest] = tmp;
idx_map.put(heap[i_1].name, i_1);
idx_map.put(heap[smallest].name, smallest);
                i_1 = smallest;
            } else {
                break;
            }
        }
mh.heap = heap;
mh.idx_of_element = idx_map;
    }

    static void sift_up(MinHeap mh, int idx) {
        Node[] heap_1 = ((Node[])(mh.heap));
        java.util.Map<String,Integer> idx_map_1 = mh.idx_of_element;
        int i_2 = idx;
        int p = get_parent_idx(i_2);
        while (p >= 0 && heap_1[p].val > heap_1[i_2].val) {
            Node tmp_1 = heap_1[p];
heap_1[p] = heap_1[i_2];
heap_1[i_2] = tmp_1;
idx_map_1.put(heap_1[p].name, p);
idx_map_1.put(heap_1[i_2].name, i_2);
            i_2 = p;
            p = get_parent_idx(i_2);
        }
mh.heap = heap_1;
mh.idx_of_element = idx_map_1;
    }

    static MinHeap new_min_heap(Node[] array) {
        java.util.Map<String,Integer> idx_map_2 = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        java.util.Map<String,Integer> val_map = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        Node[] heap_2 = ((Node[])(array));
        int i_3 = 0;
        while (i_3 < array.length) {
            Node n = array[i_3];
idx_map_2.put(n.name, i_3);
val_map.put(n.name, n.val);
            i_3 = i_3 + 1;
        }
        MinHeap mh = new MinHeap(heap_2, idx_map_2, val_map);
        int start = get_parent_idx(array.length - 1);
        while (start >= 0) {
            sift_down(mh, start);
            start = start - 1;
        }
        return mh;
    }

    static Node peek(MinHeap mh) {
        return mh.heap[0];
    }

    static Node remove_min(MinHeap mh) {
        Node[] heap_3 = ((Node[])(mh.heap));
        java.util.Map<String,Integer> idx_map_3 = mh.idx_of_element;
        java.util.Map<String,Integer> val_map_1 = mh.heap_dict;
        int last_idx = heap_3.length - 1;
        Node top = heap_3[0];
        Node last = heap_3[last_idx];
heap_3[0] = last;
idx_map_3.put(last.name, 0);
        heap_3 = ((Node[])(slice_without_last(((Node[])(heap_3)))));
        idx_map_3 = remove_key(idx_map_3, top.name);
        val_map_1 = remove_key(val_map_1, top.name);
mh.heap = heap_3;
mh.idx_of_element = idx_map_3;
mh.heap_dict = val_map_1;
        if (heap_3.length > 0) {
            sift_down(mh, 0);
        }
        return top;
    }

    static void insert(MinHeap mh, Node node) {
        Node[] heap_4 = ((Node[])(mh.heap));
        java.util.Map<String,Integer> idx_map_4 = mh.idx_of_element;
        java.util.Map<String,Integer> val_map_2 = mh.heap_dict;
        heap_4 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap_4), java.util.stream.Stream.of(node)).toArray(Node[]::new)));
        int idx = heap_4.length - 1;
idx_map_4.put(node.name, idx);
val_map_2.put(node.name, node.val);
mh.heap = heap_4;
mh.idx_of_element = idx_map_4;
mh.heap_dict = val_map_2;
        sift_up(mh, idx);
    }

    static boolean is_empty(MinHeap mh) {
        return mh.heap.length == 0;
    }

    static int get_value(MinHeap mh, String key) {
        return ((int)(mh.heap_dict).getOrDefault(key, 0));
    }

    static void decrease_key(MinHeap mh, Node node, int new_value) {
        Node[] heap_5 = ((Node[])(mh.heap));
        java.util.Map<String,Integer> val_map_3 = mh.heap_dict;
        java.util.Map<String,Integer> idx_map_5 = mh.idx_of_element;
        int idx_1 = (int)(((int)(idx_map_5).getOrDefault(node.name, 0)));
        if (!(heap_5[idx_1].val > new_value)) {
            throw new RuntimeException(String.valueOf("newValue must be less than current value"));
        }
node.val = new_value;
val_map_3.put(node.name, new_value);
mh.heap = heap_5;
mh.heap_dict = val_map_3;
        sift_up(mh, idx_1);
    }

    static String node_to_string(Node n) {
        return "Node(" + n.name + ", " + _p(n.val) + ")";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r = new Node("R", -1);
            b = new Node("B", 6);
            a = new Node("A", 3);
            x = new Node("X", 1);
            e = new Node("E", 4);
            my_min_heap = new_min_heap(((Node[])(new Node[]{r, b, a, x, e})));
            System.out.println("Min Heap - before decrease key");
            for (Node n : my_min_heap.heap) {
                System.out.println(node_to_string(n));
            }
            System.out.println("Min Heap - After decrease key of node [B -> -17]");
            decrease_key(my_min_heap, b, -17);
            for (Node n : my_min_heap.heap) {
                System.out.println(node_to_string(n));
            }
            System.out.println(_p(get_value(my_min_heap, "B")));
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
        return String.valueOf(v);
    }
}
