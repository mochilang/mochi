public class Main {
    static class Node {
        long key;
        long value;
        long prev;
        long next;
        Node(long key, long value, long prev, long next) {
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
        long head;
        long tail;
        DoubleLinkedList(Node[] nodes, long head, long tail) {
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
        long capacity;
        long num_keys;
        long hits;
        long misses;
        java.util.Map<String,Long> cache;
        LRUCache(DoubleLinkedList list, long capacity, long num_keys, long hits, long misses, java.util.Map<String,Long> cache) {
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
        long value;
        boolean ok;
        GetResult(LRUCache cache, long value, boolean ok) {
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
        Node head_1 = new Node(0, 0, (long)(0) - (long)(1), 1);
        Node tail_1 = new Node(0, 0, 0, (long)(0) - (long)(1));
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(head_1)).toArray(Node[]::new)));
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(tail_1)).toArray(Node[]::new)));
        return new DoubleLinkedList(nodes, 0, 1);
    }

    static DoubleLinkedList dll_add(DoubleLinkedList lst, long idx) {
        Node[] nodes_1 = ((Node[])(lst.nodes));
        long tail_idx_1 = (long)(lst.tail);
        Node tail_node_1 = nodes_1[(int)((long)(tail_idx_1))];
        long prev_idx_1 = (long)(tail_node_1.prev);
        Node node_1 = nodes_1[(int)((long)(idx))];
node_1.prev = prev_idx_1;
node_1.next = tail_idx_1;
nodes_1[(int)((long)(idx))] = node_1;
        Node prev_node_1 = nodes_1[(int)((long)(prev_idx_1))];
prev_node_1.next = idx;
nodes_1[(int)((long)(prev_idx_1))] = prev_node_1;
tail_node_1.prev = idx;
nodes_1[(int)((long)(tail_idx_1))] = tail_node_1;
lst.nodes = nodes_1;
        return lst;
    }

    static DoubleLinkedList dll_remove(DoubleLinkedList lst, long idx) {
        Node[] nodes_2 = ((Node[])(lst.nodes));
        Node node_3 = nodes_2[(int)((long)(idx))];
        long prev_idx_3 = (long)(node_3.prev);
        long next_idx_1 = (long)(node_3.next);
        if ((long)(prev_idx_3) == (long)((long)(0) - (long)(1)) || (long)(next_idx_1) == (long)((long)(0) - (long)(1))) {
            return lst;
        }
        Node prev_node_3 = nodes_2[(int)((long)(prev_idx_3))];
prev_node_3.next = next_idx_1;
nodes_2[(int)((long)(prev_idx_3))] = prev_node_3;
        Node next_node_1 = nodes_2[(int)((long)(next_idx_1))];
next_node_1.prev = prev_idx_3;
nodes_2[(int)((long)(next_idx_1))] = next_node_1;
node_3.prev = (long)(0) - (long)(1);
node_3.next = (long)(0) - (long)(1);
nodes_2[(int)((long)(idx))] = node_3;
lst.nodes = nodes_2;
        return lst;
    }

    static LRUCache new_cache(long cap) {
        java.util.Map<String,Long> empty_map = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        return new LRUCache(new_list(), cap, 0, 0, 0, empty_map);
    }

    static GetResult lru_get(LRUCache c, long key) {
        LRUCache cache = c;
        String key_str_1 = _p(key);
        if (cache.cache.containsKey(key_str_1)) {
            long idx_1 = (long)(((long)(cache.cache).getOrDefault(key_str_1, 0L)));
            if (idx_1 != (long)((long)(0) - (long)(1))) {
cache.hits = (long)(cache.hits) + (long)(1);
                Node node_5 = cache.list.nodes[(int)((long)(idx_1))];
                long value_1 = (long)(node_5.value);
cache.list = dll_remove(cache.list, idx_1);
cache.list = dll_add(cache.list, idx_1);
                return new GetResult(cache, value_1, true);
            }
        }
cache.misses = (long)(cache.misses) + (long)(1);
        return new GetResult(cache, 0, false);
    }

    static LRUCache lru_put(LRUCache c, long key, long value) {
        LRUCache cache_1 = c;
        String key_str_3 = _p(key);
        if (!(cache_1.cache.containsKey(key_str_3))) {
            if ((long)(cache_1.num_keys) >= (long)(cache_1.capacity)) {
                Node head_node_1 = cache_1.list.nodes[(int)((long)(cache_1.list.head))];
                long first_idx_1 = (long)(head_node_1.next);
                Node first_node_1 = cache_1.list.nodes[(int)((long)(first_idx_1))];
                long old_key_1 = (long)(first_node_1.key);
cache_1.list = dll_remove(cache_1.list, (long)(first_idx_1));
                java.util.Map<String,Long> mdel_1 = cache_1.cache;
mdel_1.put(_p(old_key_1), (long)((long)(0) - (long)(1)));
cache_1.cache = mdel_1;
cache_1.num_keys = (long)(cache_1.num_keys) - (long)(1);
            }
            Node[] nodes_5 = ((Node[])(cache_1.list.nodes));
            Node new_node_1 = new Node(key, value, (long)(0) - (long)(1), (long)(0) - (long)(1));
            nodes_5 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_5), java.util.stream.Stream.of(new_node_1)).toArray(Node[]::new)));
            long idx_4 = (long)((long)(nodes_5.length) - (long)(1));
cache_1.list.nodes = nodes_5;
cache_1.list = dll_add(cache_1.list, (long)(idx_4));
            java.util.Map<String,Long> m_2 = cache_1.cache;
m_2.put(key_str_3, (long)(idx_4));
cache_1.cache = m_2;
cache_1.num_keys = (long)(cache_1.num_keys) + (long)(1);
        } else {
            java.util.Map<String,Long> m_3 = cache_1.cache;
            long idx_5 = (long)(((long)(m_3).getOrDefault(key_str_3, 0L)));
            Node[] nodes_6 = ((Node[])(cache_1.list.nodes));
            Node node_7 = nodes_6[(int)((long)(idx_5))];
node_7.value = value;
nodes_6[(int)((long)(idx_5))] = node_7;
cache_1.list.nodes = nodes_6;
cache_1.list = dll_remove(cache_1.list, idx_5);
cache_1.list = dll_add(cache_1.list, idx_5);
cache_1.cache = m_3;
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
        LRUCache cache_2 = new_cache(2L);
        cache_2 = lru_put(cache_2, 1L, 1L);
        cache_2 = lru_put(cache_2, 2L, 2L);
        GetResult r1_1 = lru_get(cache_2, 1L);
        cache_2 = r1_1.cache;
        print_result(r1_1);
        cache_2 = lru_put(cache_2, 3L, 3L);
        GetResult r2_1 = lru_get(cache_2, 2L);
        cache_2 = r2_1.cache;
        print_result(r2_1);
        cache_2 = lru_put(cache_2, 4L, 4L);
        GetResult r3_1 = lru_get(cache_2, 1L);
        cache_2 = r3_1.cache;
        print_result(r3_1);
        GetResult r4_1 = lru_get(cache_2, 3L);
        cache_2 = r4_1.cache;
        print_result(r4_1);
        GetResult r5_1 = lru_get(cache_2, 4L);
        cache_2 = r5_1.cache;
        print_result(r5_1);
        System.out.println(cache_info(cache_2));
    }
    public static void main(String[] args) {
        main();
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
