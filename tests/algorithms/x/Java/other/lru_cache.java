public class Main {
    static class Node {
        java.math.BigInteger key;
        java.math.BigInteger value;
        java.math.BigInteger prev;
        java.math.BigInteger next;
        Node(java.math.BigInteger key, java.math.BigInteger value, java.math.BigInteger prev, java.math.BigInteger next) {
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
        java.math.BigInteger head;
        java.math.BigInteger tail;
        DoubleLinkedList(Node[] nodes, java.math.BigInteger head, java.math.BigInteger tail) {
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
        java.math.BigInteger capacity;
        java.math.BigInteger num_keys;
        java.math.BigInteger hits;
        java.math.BigInteger misses;
        java.util.Map<String,java.math.BigInteger> cache;
        LRUCache(DoubleLinkedList list, java.math.BigInteger capacity, java.math.BigInteger num_keys, java.math.BigInteger hits, java.math.BigInteger misses, java.util.Map<String,java.math.BigInteger> cache) {
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
        java.math.BigInteger value;
        boolean ok;
        GetResult(LRUCache cache, java.math.BigInteger value, boolean ok) {
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
        Node head_1 = new Node(java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), java.math.BigInteger.valueOf(1));
        Node tail_1 = new Node(java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())));
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(head_1)).toArray(Node[]::new)));
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(tail_1)).toArray(Node[]::new)));
        return new DoubleLinkedList(((Node[])(nodes)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1));
    }

    static DoubleLinkedList dll_add(DoubleLinkedList lst, java.math.BigInteger idx) {
        Node[] nodes_1 = ((Node[])(lst.nodes));
        java.math.BigInteger tail_idx_1 = new java.math.BigInteger(String.valueOf(lst.tail));
        Node tail_node_1 = nodes_1[_idx((nodes_1).length, ((java.math.BigInteger)(tail_idx_1)).longValue())];
        java.math.BigInteger prev_idx_1 = new java.math.BigInteger(String.valueOf(tail_node_1.prev));
        Node node_1 = nodes_1[_idx((nodes_1).length, ((java.math.BigInteger)(idx)).longValue())];
node_1.prev = prev_idx_1;
node_1.next = tail_idx_1;
nodes_1[(int)(((java.math.BigInteger)(idx)).longValue())] = node_1;
        Node prev_node_1 = nodes_1[_idx((nodes_1).length, ((java.math.BigInteger)(prev_idx_1)).longValue())];
prev_node_1.next = idx;
nodes_1[(int)(((java.math.BigInteger)(prev_idx_1)).longValue())] = prev_node_1;
tail_node_1.prev = idx;
nodes_1[(int)(((java.math.BigInteger)(tail_idx_1)).longValue())] = tail_node_1;
lst.nodes = nodes_1;
        return lst;
    }

    static DoubleLinkedList dll_remove(DoubleLinkedList lst, java.math.BigInteger idx) {
        Node[] nodes_2 = ((Node[])(lst.nodes));
        Node node_3 = nodes_2[_idx((nodes_2).length, ((java.math.BigInteger)(idx)).longValue())];
        java.math.BigInteger prev_idx_3 = new java.math.BigInteger(String.valueOf(node_3.prev));
        java.math.BigInteger next_idx_1 = new java.math.BigInteger(String.valueOf(node_3.next));
        if (prev_idx_3.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0 || next_idx_1.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            return lst;
        }
        Node prev_node_3 = nodes_2[_idx((nodes_2).length, ((java.math.BigInteger)(prev_idx_3)).longValue())];
prev_node_3.next = next_idx_1;
nodes_2[(int)(((java.math.BigInteger)(prev_idx_3)).longValue())] = prev_node_3;
        Node next_node_1 = nodes_2[_idx((nodes_2).length, ((java.math.BigInteger)(next_idx_1)).longValue())];
next_node_1.prev = prev_idx_3;
nodes_2[(int)(((java.math.BigInteger)(next_idx_1)).longValue())] = next_node_1;
node_3.prev = (java.math.BigInteger.valueOf(1)).negate();
node_3.next = (java.math.BigInteger.valueOf(1)).negate();
nodes_2[(int)(((java.math.BigInteger)(idx)).longValue())] = node_3;
lst.nodes = nodes_2;
        return lst;
    }

    static LRUCache new_cache(java.math.BigInteger cap) {
        java.util.Map<String,java.math.BigInteger> empty_map = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>()));
        return new LRUCache(new_list(), new java.math.BigInteger(String.valueOf(cap)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), empty_map);
    }

    static GetResult lru_get(LRUCache c, java.math.BigInteger key) {
        LRUCache cache = c;
        String key_str_1 = _p(key);
        if (cache.cache.containsKey(key_str_1)) {
            java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(cache.cache).get(key_str_1))));
            if (idx_1.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0) {
cache.hits = cache.hits.add(java.math.BigInteger.valueOf(1));
                Node node_5 = cache.list.nodes[_idx((cache.list.nodes).length, ((java.math.BigInteger)(idx_1)).longValue())];
                java.math.BigInteger value_1 = new java.math.BigInteger(String.valueOf(node_5.value));
cache.list = dll_remove(cache.list, new java.math.BigInteger(String.valueOf(idx_1)));
cache.list = dll_add(cache.list, new java.math.BigInteger(String.valueOf(idx_1)));
                return new GetResult(cache, new java.math.BigInteger(String.valueOf(value_1)), true);
            }
        }
cache.misses = cache.misses.add(java.math.BigInteger.valueOf(1));
        return new GetResult(cache, java.math.BigInteger.valueOf(0), false);
    }

    static LRUCache lru_put(LRUCache c, java.math.BigInteger key, java.math.BigInteger value) {
        LRUCache cache_1 = c;
        String key_str_3 = _p(key);
        if (!(cache_1.cache.containsKey(key_str_3))) {
            if (cache_1.num_keys.compareTo(cache_1.capacity) >= 0) {
                Node head_node_1 = cache_1.list.nodes[_idx((cache_1.list.nodes).length, ((java.math.BigInteger)(cache_1.list.head)).longValue())];
                java.math.BigInteger first_idx_1 = new java.math.BigInteger(String.valueOf(head_node_1.next));
                Node first_node_1 = cache_1.list.nodes[_idx((cache_1.list.nodes).length, ((java.math.BigInteger)(first_idx_1)).longValue())];
                java.math.BigInteger old_key_1 = new java.math.BigInteger(String.valueOf(first_node_1.key));
cache_1.list = dll_remove(cache_1.list, new java.math.BigInteger(String.valueOf(first_idx_1)));
                java.util.Map<String,java.math.BigInteger> mdel_1 = cache_1.cache;
mdel_1.put(_p(old_key_1), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())));
cache_1.cache = mdel_1;
cache_1.num_keys = cache_1.num_keys.subtract(java.math.BigInteger.valueOf(1));
            }
            Node[] nodes_5 = ((Node[])(cache_1.list.nodes));
            Node new_node_1 = new Node(new java.math.BigInteger(String.valueOf(key)), new java.math.BigInteger(String.valueOf(value)), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())));
            nodes_5 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_5), java.util.stream.Stream.of(new_node_1)).toArray(Node[]::new)));
            java.math.BigInteger idx_4 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(nodes_5.length)).subtract(java.math.BigInteger.valueOf(1))));
cache_1.list.nodes = nodes_5;
cache_1.list = dll_add(cache_1.list, new java.math.BigInteger(String.valueOf(idx_4)));
            java.util.Map<String,java.math.BigInteger> m_2 = cache_1.cache;
m_2.put(key_str_3, new java.math.BigInteger(String.valueOf(idx_4)));
cache_1.cache = m_2;
cache_1.num_keys = cache_1.num_keys.add(java.math.BigInteger.valueOf(1));
        } else {
            java.util.Map<String,java.math.BigInteger> m_3 = cache_1.cache;
            java.math.BigInteger idx_5 = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(m_3).get(key_str_3))));
            Node[] nodes_6 = ((Node[])(cache_1.list.nodes));
            Node node_7 = nodes_6[_idx((nodes_6).length, ((java.math.BigInteger)(idx_5)).longValue())];
node_7.value = value;
nodes_6[(int)(((java.math.BigInteger)(idx_5)).longValue())] = node_7;
cache_1.list.nodes = nodes_6;
cache_1.list = dll_remove(cache_1.list, new java.math.BigInteger(String.valueOf(idx_5)));
cache_1.list = dll_add(cache_1.list, new java.math.BigInteger(String.valueOf(idx_5)));
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
        LRUCache cache_2 = new_cache(java.math.BigInteger.valueOf(2));
        cache_2 = lru_put(cache_2, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1));
        cache_2 = lru_put(cache_2, java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(2));
        GetResult r1_1 = lru_get(cache_2, java.math.BigInteger.valueOf(1));
        cache_2 = r1_1.cache;
        print_result(r1_1);
        cache_2 = lru_put(cache_2, java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(3));
        GetResult r2_1 = lru_get(cache_2, java.math.BigInteger.valueOf(2));
        cache_2 = r2_1.cache;
        print_result(r2_1);
        cache_2 = lru_put(cache_2, java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(4));
        GetResult r3_1 = lru_get(cache_2, java.math.BigInteger.valueOf(1));
        cache_2 = r3_1.cache;
        print_result(r3_1);
        GetResult r4_1 = lru_get(cache_2, java.math.BigInteger.valueOf(3));
        cache_2 = r4_1.cache;
        print_result(r4_1);
        GetResult r5_1 = lru_get(cache_2, java.math.BigInteger.valueOf(4));
        cache_2 = r5_1.cache;
        print_result(r5_1);
        System.out.println(cache_info(cache_2));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
