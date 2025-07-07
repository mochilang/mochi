import java.util.*;

class Item {
    String cat;
    int val;
    boolean flag;
    Item(String cat, int val, boolean flag) {
        this.cat = cat;
        this.val = val;
        this.flag = flag;
    }
}

public class GroupByConditionalSum {
    public static void main(String[] args) {
        List<Item> items = Arrays.asList(
            new Item("a", 10, true),
            new Item("a", 5, false),
            new Item("b", 20, true)
        );

        Map<String, List<Item>> groups = new HashMap<>();
        for (Item i : items) {
            groups.computeIfAbsent(i.cat, k -> new ArrayList<>()).add(i);
        }

        List<Map<String, Object>> result = new ArrayList<>();
        for (String cat : groups.keySet()) {
            List<Item> g = groups.get(cat);
            double sum = 0;
            double cond = 0;
            for (Item x : g) {
                sum += x.val;
                if (x.flag) {
                    cond += x.val;
                }
            }
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("cat", cat);
            m.put("share", cond / sum);
            result.add(m);
        }
        result.sort(Comparator.comparing(m -> (String)m.get("cat")));
        System.out.println(result);
    }
}
