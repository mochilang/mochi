import java.util.*;

class IdName {
    int id;
    String name;
    IdName(int id, String name) {
        this.id = id;
        this.name = name;
    }
    int size() { return 2; }
}
class IdCustomerId {
    int id;
    int customerId;
    IdCustomerId(int id, int customerId) {
        this.id = id;
        this.customerId = customerId;
    }
    int size() { return 2; }
}
class NameCount {
    Object name;
    int count;
    NameCount(Object name, int count) {
        this.name = name;
        this.count = count;
    }
    int size() { return 2; }
}
class CO {
    IdName c;
    IdCustomerId o;
    CO(IdName c, IdCustomerId o) {
        this.c = c;
        this.o = o;
    }
    int size() { return 2; }
}
public class GroupByLeftJoin {
    public static void main(String[] args) {
    List<IdName> customers = new ArrayList<>(Arrays.asList(new IdName(1, "Alice"), new IdName(2, "Bob"), new IdName(3, "Charlie")));
    List<IdCustomerId> orders = new ArrayList<>(Arrays.asList(new IdCustomerId(100, 1), new IdCustomerId(101, 1), new IdCustomerId(102, 2)));
    List<NameCount> stats = (new java.util.function.Supplier<List<NameCount>>(){public List<NameCount> get(){
    List<NameCount> _res0 = new ArrayList<>();
    Map<String,List<CO>> _groups1 = new LinkedHashMap<>();
    for (var c : customers) {
        List<IdCustomerId> _tmp2 = new ArrayList<>();
        for (var _it3 : orders) {
            var o = _it3;
            if (!(Objects.equals(o.customerId, c.id))) continue;
            _tmp2.add(_it3);
        }
        if (_tmp2.isEmpty()) _tmp2.add(null);
        for (var o : _tmp2) {
            CO _row4 = new CO(c, o);
            String _key5 = c.name;
            List<CO> _b6 = _groups1.get(_key5);
            if (_b6 == null) { _b6 = new ArrayList<>(); _groups1.put(_key5, _b6); }
            _b6.add(_row4);
        }
    }
    for (Map.Entry<String,List<CO>> __e : _groups1.entrySet()) {
        String g_key = __e.getKey();
        List<CO> g = __e.getValue();
        _res0.add(new NameCount(g_key, (new java.util.function.Supplier<List<CO>>(){public List<CO> get(){
    List<CO> _res7 = new ArrayList<>();
    for (var r : g) {
        if (!(r.o != null)) continue;
        _res7.add(r);
    }
    return _res7;
}}).get().size()));
    }
    return _res0;
}}).get();
    System.out.println("--- Group Left Join ---");
    for (NameCount s : stats) {
        System.out.println(s.name + " " + "orders:" + " " + s.count);
    }
    }
}
