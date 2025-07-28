public class Main {
    static class dlNode {
        int value;
        Object next;
        Object prev;
        dlNode(int value, Object next, Object prev) {
            this.value = value;
            this.next = next;
            this.prev = prev;
        }
        @Override public String toString() {
            return String.format("{'value': %s, 'next': %s, 'prev': %s}", String.valueOf(value), String.valueOf(next), String.valueOf(prev));
        }
    }

    static class dlList {
        java.util.Map<Object,Integer> members;
        Object head;
        Object tail;
        dlList(java.util.Map<Object,Integer> members, Object head, Object tail) {
            this.members = members;
            this.head = head;
            this.tail = tail;
        }
        @Override public String toString() {
            return String.format("{'members': %s, 'head': %s, 'tail': %s}", String.valueOf(members), String.valueOf(head), String.valueOf(tail));
        }
    }


    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
}
