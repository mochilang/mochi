import java.util.*;

class NNationkeyNName {
    int n_nationkey;
    String n_name;
    NNationkeyNName(int n_nationkey, String n_name) {
        this.n_nationkey = n_nationkey;
        this.n_name = n_name;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NNationkeyNName other)) return false;
        return Objects.equals(this.n_nationkey, other.n_nationkey) && Objects.equals(this.n_name, other.n_name);
    }
    @Override public int hashCode() {
        return Objects.hash(n_nationkey, n_name);
    }
    int size() { return 2; }
}
class CCustkeyCNameCAcctbalCNationkeyCAddressCPhoneCComment {
    int c_custkey;
    String c_name;
    double c_acctbal;
    int c_nationkey;
    String c_address;
    String c_phone;
    String c_comment;
    CCustkeyCNameCAcctbalCNationkeyCAddressCPhoneCComment(int c_custkey, String c_name, double c_acctbal, int c_nationkey, String c_address, String c_phone, String c_comment) {
        this.c_custkey = c_custkey;
        this.c_name = c_name;
        this.c_acctbal = c_acctbal;
        this.c_nationkey = c_nationkey;
        this.c_address = c_address;
        this.c_phone = c_phone;
        this.c_comment = c_comment;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CCustkeyCNameCAcctbalCNationkeyCAddressCPhoneCComment other)) return false;
        return Objects.equals(this.c_custkey, other.c_custkey) && Objects.equals(this.c_name, other.c_name) && Objects.equals(this.c_acctbal, other.c_acctbal) && Objects.equals(this.c_nationkey, other.c_nationkey) && Objects.equals(this.c_address, other.c_address) && Objects.equals(this.c_phone, other.c_phone) && Objects.equals(this.c_comment, other.c_comment);
    }
    @Override public int hashCode() {
        return Objects.hash(c_custkey, c_name, c_acctbal, c_nationkey, c_address, c_phone, c_comment);
    }
    int size() { return 7; }
}
class OOrderkeyOCustkeyOOrderdate {
    int o_orderkey;
    int o_custkey;
    String o_orderdate;
    OOrderkeyOCustkeyOOrderdate(int o_orderkey, int o_custkey, String o_orderdate) {
        this.o_orderkey = o_orderkey;
        this.o_custkey = o_custkey;
        this.o_orderdate = o_orderdate;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OOrderkeyOCustkeyOOrderdate other)) return false;
        return Objects.equals(this.o_orderkey, other.o_orderkey) && Objects.equals(this.o_custkey, other.o_custkey) && Objects.equals(this.o_orderdate, other.o_orderdate);
    }
    @Override public int hashCode() {
        return Objects.hash(o_orderkey, o_custkey, o_orderdate);
    }
    int size() { return 3; }
}
class LOrderkeyLReturnflagLExtendedpriceLDiscount {
    int l_orderkey;
    String l_returnflag;
    double l_extendedprice;
    double l_discount;
    LOrderkeyLReturnflagLExtendedpriceLDiscount(int l_orderkey, String l_returnflag, double l_extendedprice, double l_discount) {
        this.l_orderkey = l_orderkey;
        this.l_returnflag = l_returnflag;
        this.l_extendedprice = l_extendedprice;
        this.l_discount = l_discount;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LOrderkeyLReturnflagLExtendedpriceLDiscount other)) return false;
        return Objects.equals(this.l_orderkey, other.l_orderkey) && Objects.equals(this.l_returnflag, other.l_returnflag) && Objects.equals(this.l_extendedprice, other.l_extendedprice) && Objects.equals(this.l_discount, other.l_discount);
    }
    @Override public int hashCode() {
        return Objects.hash(l_orderkey, l_returnflag, l_extendedprice, l_discount);
    }
    int size() { return 4; }
}
class CCustkeyCNameRevenueCAcctbalNNameCAddressCPhoneCComment {
    Object c_custkey;
    Object c_name;
    int revenue;
    Object c_acctbal;
    Object n_name;
    Object c_address;
    Object c_phone;
    Object c_comment;
    CCustkeyCNameRevenueCAcctbalNNameCAddressCPhoneCComment(Object c_custkey, Object c_name, int revenue, Object c_acctbal, Object n_name, Object c_address, Object c_phone, Object c_comment) {
        this.c_custkey = c_custkey;
        this.c_name = c_name;
        this.revenue = revenue;
        this.c_acctbal = c_acctbal;
        this.n_name = n_name;
        this.c_address = c_address;
        this.c_phone = c_phone;
        this.c_comment = c_comment;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CCustkeyCNameRevenueCAcctbalNNameCAddressCPhoneCComment other)) return false;
        return Objects.equals(this.c_custkey, other.c_custkey) && Objects.equals(this.c_name, other.c_name) && Objects.equals(this.revenue, other.revenue) && Objects.equals(this.c_acctbal, other.c_acctbal) && Objects.equals(this.n_name, other.n_name) && Objects.equals(this.c_address, other.c_address) && Objects.equals(this.c_phone, other.c_phone) && Objects.equals(this.c_comment, other.c_comment);
    }
    @Override public int hashCode() {
        return Objects.hash(c_custkey, c_name, revenue, c_acctbal, n_name, c_address, c_phone, c_comment);
    }
    int size() { return 8; }
}
class COLN {
    CCustkeyCNameCAcctbalCNationkeyCAddressCPhoneCComment c;
    OOrderkeyOCustkeyOOrderdate o;
    LOrderkeyLReturnflagLExtendedpriceLDiscount l;
    NNationkeyNName n;
    COLN(CCustkeyCNameCAcctbalCNationkeyCAddressCPhoneCComment c, OOrderkeyOCustkeyOOrderdate o, LOrderkeyLReturnflagLExtendedpriceLDiscount l, NNationkeyNName n) {
        this.c = c;
        this.o = o;
        this.l = l;
        this.n = n;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof COLN other)) return false;
        return Objects.equals(this.c, other.c) && Objects.equals(this.o, other.o) && Objects.equals(this.l, other.l) && Objects.equals(this.n, other.n);
    }
    @Override public int hashCode() {
        return Objects.hash(c, o, l, n);
    }
    int size() { return 4; }
}
class CCustkeyCNameCAcctbalCAddressCPhoneCCommentNName {
    int c_custkey;
    String c_name;
    double c_acctbal;
    String c_address;
    String c_phone;
    String c_comment;
    String n_name;
    CCustkeyCNameCAcctbalCAddressCPhoneCCommentNName(int c_custkey, String c_name, double c_acctbal, String c_address, String c_phone, String c_comment, String n_name) {
        this.c_custkey = c_custkey;
        this.c_name = c_name;
        this.c_acctbal = c_acctbal;
        this.c_address = c_address;
        this.c_phone = c_phone;
        this.c_comment = c_comment;
        this.n_name = n_name;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CCustkeyCNameCAcctbalCAddressCPhoneCCommentNName other)) return false;
        return Objects.equals(this.c_custkey, other.c_custkey) && Objects.equals(this.c_name, other.c_name) && Objects.equals(this.c_acctbal, other.c_acctbal) && Objects.equals(this.c_address, other.c_address) && Objects.equals(this.c_phone, other.c_phone) && Objects.equals(this.c_comment, other.c_comment) && Objects.equals(this.n_name, other.n_name);
    }
    @Override public int hashCode() {
        return Objects.hash(c_custkey, c_name, c_acctbal, c_address, c_phone, c_comment, n_name);
    }
    int size() { return 7; }
}
public class GroupByMultiJoinSort {
    public static void main(String[] args) {
    List<NNationkeyNName> nation = new ArrayList<>(Arrays.asList(new NNationkeyNName(1, "BRAZIL")));
    List<CCustkeyCNameCAcctbalCNationkeyCAddressCPhoneCComment> customer = new ArrayList<>(Arrays.asList(new CCustkeyCNameCAcctbalCNationkeyCAddressCPhoneCComment(1, "Alice", 100.000000, 1, "123 St", "123-456", "Loyal")));
    List<OOrderkeyOCustkeyOOrderdate> orders = new ArrayList<>(Arrays.asList(new OOrderkeyOCustkeyOOrderdate(1000, 1, "1993-10-15"), new OOrderkeyOCustkeyOOrderdate(2000, 1, "1994-01-02")));
    List<LOrderkeyLReturnflagLExtendedpriceLDiscount> lineitem = new ArrayList<>(Arrays.asList(new LOrderkeyLReturnflagLExtendedpriceLDiscount(1000, "R", 1000.000000, 0.100000), new LOrderkeyLReturnflagLExtendedpriceLDiscount(2000, "N", 500.000000, 0.000000)));
    String start_date = "1993-10-01";
    String end_date = "1994-01-01";
    List<CCustkeyCNameRevenueCAcctbalNNameCAddressCPhoneCComment> result = (new java.util.function.Supplier<List<CCustkeyCNameRevenueCAcctbalNNameCAddressCPhoneCComment>>(){public List<CCustkeyCNameRevenueCAcctbalNNameCAddressCPhoneCComment> get(){
    List<CCustkeyCNameRevenueCAcctbalNNameCAddressCPhoneCComment> _res0 = new ArrayList<>();
    Map<CCustkeyCNameCAcctbalCAddressCPhoneCCommentNName,List<COLN>> _groups1 = new LinkedHashMap<>();
    for (var c : customer) {
        for (var o : orders) {
            if (!(Objects.equals(o.o_custkey, c.c_custkey))) continue;
            for (var l : lineitem) {
                if (!(Objects.equals(l.l_orderkey, o.o_orderkey))) continue;
                for (var n : nation) {
                    if (!(Objects.equals(n.n_nationkey, c.c_nationkey))) continue;
                    if (!(Objects.equals(String.valueOf(String.valueOf(o.o_orderdate).compareTo(String.valueOf(start_date)) >= 0 && o.o_orderdate != null).compareTo(String.valueOf(end_date)) < 0 && l.l_returnflag != null, "R"))) continue;
                    COLN _row2 = new COLN(c, o, l, n);
                    CCustkeyCNameCAcctbalCAddressCPhoneCCommentNName _key3 = new CCustkeyCNameCAcctbalCAddressCPhoneCCommentNName(c.c_custkey, c.c_name, c.c_acctbal, c.c_address, c.c_phone, c.c_comment, n.n_name);
                    List<COLN> _b4 = _groups1.get(_key3);
                    if (_b4 == null) { _b4 = new ArrayList<>(); _groups1.put(_key3, _b4); }
                    _b4.add(_row2);
                }
            }
        }
    }
    for (Map.Entry<CCustkeyCNameCAcctbalCAddressCPhoneCCommentNName,List<COLN>> __e : _groups1.entrySet()) {
        CCustkeyCNameCAcctbalCAddressCPhoneCCommentNName g_key = __e.getKey();
        List<COLN> g = __e.getValue();
        _res0.add(new CCustkeyCNameRevenueCAcctbalNNameCAddressCPhoneCComment(g_key.c_custkey, g_key.c_name, (new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
    List<Double> _res5 = new ArrayList<>();
    for (var x : g) {
        _res5.add(x.l.l_extendedprice * (1 - x.l.l_discount));
    }
    return _res5;
}}).get().stream().mapToInt(n -> ((Number)n).intValue()).sum(), g_key.c_acctbal, g_key.n_name, g_key.c_address, g_key.c_phone, g_key.c_comment));
    }
    return _res0;
}}).get();
    System.out.println(result);
    }
}
