public class Main {
    static class Item {
        int weight;
        int value;
        Item(int weight, int value) {
            this.weight = weight;
            this.value = value;
        }
        Item() {}
        @Override public String toString() {
            return String.format("{'weight': %s, 'value': %s}", String.valueOf(weight), String.valueOf(value));
        }
    }

    static Item[] items1;
    static Item[] items2;
    static Item[] items3;
    static Item[] items4;

    static double ratio(Item item) {
        return (((Number)(item.value)).doubleValue()) / (((Number)(item.weight)).doubleValue());
    }

    static double fractional_cover(Item[] items, int capacity) {
        if (capacity < 0) {
            throw new RuntimeException(String.valueOf("Capacity cannot be negative"));
        }
        double total = 0.0;
        int remaining = capacity;
        java.util.List<Item> sorted = new java.util.ArrayList<Item>() {{ java.util.ArrayList<Item> _tmp = new java.util.ArrayList<>(); for (var it : items) { _tmp.add(it); } java.util.ArrayList<Item> list = _tmp; java.util.ArrayList<Item> _res = new java.util.ArrayList<>(); list.sort((a, b) -> {Comparable _va = (Comparable)(ratio(a)); Comparable _vb = (Comparable)(ratio(b)); return _vb.compareTo(_va);}); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Item)list.get(i)); } addAll(_res);}};
        int idx = 0;
        while (idx < String.valueOf(sorted).length() && remaining > 0) {
            Item item = sorted[idx];
            int take = item.weight < remaining ? item.weight : remaining;
            total = total + (((Number)(take)).doubleValue()) * ratio(item);
            remaining = remaining - take;
            idx = idx + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            items1 = ((Item[])(new Item[]{new Item(10, 60), new Item(20, 100), new Item(30, 120)}));
            System.out.println(_p(fractional_cover(((Item[])(items1)), 50)));
            items2 = ((Item[])(new Item[]{new Item(20, 100), new Item(30, 120), new Item(10, 60)}));
            System.out.println(_p(fractional_cover(((Item[])(items2)), 25)));
            items3 = ((Item[])(new Item[]{}));
            System.out.println(_p(fractional_cover(((Item[])(items3)), 50)));
            items4 = ((Item[])(new Item[]{new Item(10, 60)}));
            System.out.println(_p(fractional_cover(((Item[])(items4)), 5)));
            System.out.println(_p(fractional_cover(((Item[])(items4)), 1)));
            System.out.println(_p(fractional_cover(((Item[])(items4)), 0)));
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
