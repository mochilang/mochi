public class Main {
    static class Item {
        long weight;
        long value;
        Item(long weight, long value) {
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

    static double fractional_cover(Item[] items, long capacity) {
        if (capacity < 0) {
            throw new RuntimeException(String.valueOf("Capacity cannot be negative"));
        }
        double total_1 = 0.0;
        long remaining_1 = capacity;
        java.util.List<Item> sorted_1 = new java.util.ArrayList<Item>() {{ java.util.ArrayList<Item> _tmp = new java.util.ArrayList<>(); for (var it : items) { _tmp.add(it); } java.util.ArrayList<Item> list = _tmp; java.util.ArrayList<Item> _res = new java.util.ArrayList<>(); list.sort((a, b) -> {Comparable _va = (Comparable)(ratio(a)); Comparable _vb = (Comparable)(ratio(b)); return _vb.compareTo(_va);}); int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _res.add((Item)list.get(i)); } addAll(_res);}};
        long idx_1 = 0L;
        while (idx_1 < String.valueOf(sorted_1).length() && remaining_1 > 0) {
            Item item_1 = sorted_1.get((int)((long)(idx_1)));
            long take_1 = item_1.weight < remaining_1 ? item_1.weight : remaining_1;
            total_1 = total_1 + (((Number)(take_1)).doubleValue()) * ratio(item_1);
            remaining_1 = remaining_1 - take_1;
            idx_1 = idx_1 + 1;
        }
        return total_1;
    }
    public static void main(String[] args) {
        items1 = ((Item[])(new Item[]{new Item(10, 60), new Item(20, 100), new Item(30, 120)}));
        System.out.println(_p(fractional_cover(((Item[])(items1)), 50L)));
        items2 = ((Item[])(new Item[]{new Item(20, 100), new Item(30, 120), new Item(10, 60)}));
        System.out.println(_p(fractional_cover(((Item[])(items2)), 25L)));
        items3 = ((Item[])(new Item[]{}));
        System.out.println(_p(fractional_cover(((Item[])(items3)), 50L)));
        items4 = ((Item[])(new Item[]{new Item(10, 60)}));
        System.out.println(_p(fractional_cover(((Item[])(items4)), 5L)));
        System.out.println(_p(fractional_cover(((Item[])(items4)), 1L)));
        System.out.println(_p(fractional_cover(((Item[])(items4)), 0L)));
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
