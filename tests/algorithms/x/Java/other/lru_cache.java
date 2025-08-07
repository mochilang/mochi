public class Main {
    static class Node {
        int key;
        int value;
        int prev;
        int next;
        Node(int key, int value, int prev, int next) {
            this.key = key;
            this.value = value;
            this.prev = prev;
            this.next = next;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'key': %s, 'value': %s, 'prev': %s, 'next': %s}", String.valueOf(key), String.valueOf(value), String.valueOf(prev), String.valueOf(next));
        }
    }

    static class DoubleLinkedList {
        Node[] nodes;
        int head;
        int tail;
        DoubleLinkedList(Node[] nodes, int head, int tail) {
            this.nodes = nodes;
            this.head = head;
            this.tail = tail;
        }
        DoubleLinkedList() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'head': %s, 'tail': %s}", String.valueOf(nodes), String.valueOf(head), String.valueOf(tail));
        }
    }

    static class LRUCache {
        DoubleLinkedList list;
        int capacity;
        int num_keys;
        int hits;
        int misses;
        java.util.Map<String,Integer> cache;
        LRUCache(DoubleLinkedList list, int capacity, int num_keys, int hits, int misses, java.util.Map<String,Integer> cache) {
            this.list = list;
            this.capacity = capacity;
            this.num_keys = num_keys;
            this.hits = hits;
            this.misses = misses;
            this.cache = cache;
        }
        LRUCache() {}
        @Override public String toString() {
            return String.format("{'list': %s, 'capacity': %s, 'num_keys': %s, 'hits': %s, 'misses': %s, 'cache': %s}", String.valueOf(list), String.valueOf(capacity), String.valueOf(num_keys), String.valueOf(hits), String.valueOf(misses), String.valueOf(cache));
        }
    }

    static class GetResult {
        LRUCache cache;
        int value;
        boolean ok;
        GetResult(LRUCache cache, int value, boolean ok) {
            this.cache = cache;
            this.value = value;
            this.ok = ok;
        }
        GetResult() {}
        @Override public String toString() {
            return String.format("{'cache': %s, 'value': %s, 'ok': %s}", String.valueOf(cache), String.valueOf(value), String.valueOf(ok));
        }
    }


    static DoubleLinkedList new_list() {
        Node[] nodes = ((Node[])(new Node[]{}));
        Node head = new Node(0, 0, 0 - 1, 1);
        Node tail = new Node(0, 0, 0, 0 - 1);
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(head)).toArray(Node[]::new)));
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(tail)).toArray(Node[]::new)));
        return new DoubleLinkedList(nodes, 0, 1);
    }

    static DoubleLinkedList dll_add(DoubleLinkedList lst, int idx) {
        Node[] nodes_1 = ((Node[])(lst.nodes));
        int tail_idx = lst.tail;
        Node tail_node = nodes_1[tail_idx];
        int prev_idx = tail_node.prev;
        Node node = nodes_1[idx];
node.prev = prev_idx;
node.next = tail_idx;
nodes_1[idx] = node;
        Node prev_node = nodes_1[prev_idx];
prev_node.next = idx;
nodes_1[prev_idx] = prev_node;
tail_node.prev = idx;
nodes_1[tail_idx] = tail_node;
lst.nodes = nodes_1;
        return lst;
    }

    static DoubleLinkedList dll_remove(DoubleLinkedList lst, int idx) {
        Node[] nodes_2 = ((Node[])(lst.nodes));
        Node node_1 = nodes_2[idx];
        int prev_idx_1 = node_1.prev;
        int next_idx = node_1.next;
        if (prev_idx_1 == 0 - 1 || next_idx == 0 - 1) {
            return lst;
        }
        Node prev_node_1 = nodes_2[prev_idx_1];
prev_node_1.next = next_idx;
nodes_2[prev_idx_1] = prev_node_1;
        Node next_node = nodes_2[next_idx];
next_node.prev = prev_idx_1;
nodes_2[next_idx] = next_node;
node_1.prev = 0 - 1;
node_1.next = 0 - 1;
nodes_2[idx] = node_1;
lst.nodes = nodes_2;
        return lst;
    }

    static LRUCache new_cache(int cap) {
        java.util.Map<String,Integer> empty_map = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        return new LRUCache(new_list(), cap, 0, 0, 0, empty_map);
    }

    static GetResult lru_get(LRUCache c, int key) {
        LRUCache cache = c;
        String key_str = _p(key);
        if (((Boolean)(cache.cache.containsKey(key_str)))) {
            int idx = (int)(((int)(cache.cache).getOrDefault(key_str, 0)));
            if (idx != 0 - 1) {
cache.hits = cache.hits + 1;
                Node node_2 = cache.list.nodes[idx];
                int value = node_2.value;
cache.list = dll_remove(cache.list, idx);
cache.list = dll_add(cache.list, idx);
                return new GetResult(cache, value, true);
            }
        }
cache.misses = cache.misses + 1;
        return new GetResult(cache, 0, false);
    }

    static LRUCache lru_put(LRUCache c, int key, int value) {
        LRUCache cache_1 = c;
        String key_str_1 = _p(key);
        if (!(Boolean)(cache_1.cache.containsKey(key_str_1))) {
            if (cache_1.num_keys >= cache_1.capacity) {
                Node head_node = cache_1.list.nodes[cache_1.list.head];
                int first_idx = head_node.next;
                Node first_node = cache_1.list.nodes[first_idx];
                int old_key = first_node.key;
cache_1.list = dll_remove(cache_1.list, first_idx);
                java.util.Map<String,Integer> mdel = cache_1.cache;
mdel.put(_p(old_key), 0 - 1);
cache_1.cache = mdel;
cache_1.num_keys = cache_1.num_keys - 1;
            }
            Node[] nodes_3 = ((Node[])(cache_1.list.nodes));
            Node new_node = new Node(key, value, 0 - 1, 0 - 1);
            nodes_3 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_3), java.util.stream.Stream.of(new_node)).toArray(Node[]::new)));
            int idx_1 = nodes_3.length - 1;
cache_1["list"].nodes = nodes_3;
cache_1.list = dll_add(cache_1.list, idx_1);
            java.util.Map<String,Integer> m = cache_1.cache;
m.put(key_str_1, idx_1);
cache_1.cache = m;
cache_1.num_keys = cache_1.num_keys + 1;
        } else {
            java.util.Map<String,Integer> m_1 = cache_1.cache;
            int idx_2 = (int)(((int)(m_1).getOrDefault(key_str_1, 0)));
            Node[] nodes_4 = ((Node[])(cache_1.list.nodes));
            Node node_3 = nodes_4[idx_2];
node_3.value = value;
nodes_4[idx_2] = node_3;
cache_1["list"].nodes = nodes_4;
cache_1.list = dll_remove(cache_1.list, idx_2);
cache_1.list = dll_add(cache_1.list, idx_2);
cache_1.cache = m_1;
        }
        return cache_1;
    }

    static String cache_info(LRUCache cache) {
        return "CacheInfo(hits=" + _p(cache.hits) + ", misses=" + _p(cache.misses) + ", capacity=" + _p(cache.capacity) + ", current size=" + _p(cache.num_keys) + ")";
    }

    static void print_result(GetResult res) {
        if (res.ok) {
            System.out.println(_p(res.value));
        } else {
            System.out.println("None");
        }
    }

    static void main() {
        LRUCache cache_2 = new_cache(2);
        cache_2 = lru_put(cache_2, 1, 1);
        cache_2 = lru_put(cache_2, 2, 2);
        GetResult r1 = lru_get(cache_2, 1);
        cache_2 = r1.cache;
        print_result(r1);
        cache_2 = lru_put(cache_2, 3, 3);
        GetResult r2 = lru_get(cache_2, 2);
        cache_2 = r2.cache;
        print_result(r2);
        cache_2 = lru_put(cache_2, 4, 4);
        GetResult r3 = lru_get(cache_2, 1);
        cache_2 = r3.cache;
        print_result(r3);
        GetResult r4 = lru_get(cache_2, 3);
        cache_2 = r4.cache;
        print_result(r4);
        GetResult r5 = lru_get(cache_2, 4);
        cache_2 = r5.cache;
        print_result(r5);
        System.out.println(cache_info(cache_2));
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
