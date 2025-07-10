import java.util.*;
class DataClass1 {
	int n_nationkey;
	String n_name;
	DataClass1(int n_nationkey, String n_name) {
		this.n_nationkey = n_nationkey;
		this.n_name = n_name;
	}
}
class DataClass2 {
	int c_custkey;
	String c_name;
	double c_acctbal;
	int c_nationkey;
	String c_address;
	String c_phone;
	String c_comment;
	DataClass2(int c_custkey, String c_name, double c_acctbal, int c_nationkey, String c_address, String c_phone, String c_comment) {
		this.c_custkey = c_custkey;
		this.c_name = c_name;
		this.c_acctbal = c_acctbal;
		this.c_nationkey = c_nationkey;
		this.c_address = c_address;
		this.c_phone = c_phone;
		this.c_comment = c_comment;
	}
}
class DataClass3 {
	int o_orderkey;
	int o_custkey;
	String o_orderdate;
	DataClass3(int o_orderkey, int o_custkey, String o_orderdate) {
		this.o_orderkey = o_orderkey;
		this.o_custkey = o_custkey;
		this.o_orderdate = o_orderdate;
	}
}
class DataClass4 {
	int l_orderkey;
	String l_returnflag;
	double l_extendedprice;
	double l_discount;
	DataClass4(int l_orderkey, String l_returnflag, double l_extendedprice, double l_discount) {
		this.l_orderkey = l_orderkey;
		this.l_returnflag = l_returnflag;
		this.l_extendedprice = l_extendedprice;
		this.l_discount = l_discount;
	}
}
class DataClass5 {
	Object c_custkey;
	Object c_name;
	int revenue;
	Object c_acctbal;
	Object n_name;
	Object c_address;
	Object c_phone;
	Object c_comment;
	DataClass5(Object c_custkey, Object c_name, int revenue, Object c_acctbal, Object n_name, Object c_address, Object c_phone, Object c_comment) {
		this.c_custkey = c_custkey;
		this.c_name = c_name;
		this.revenue = revenue;
		this.c_acctbal = c_acctbal;
		this.n_name = n_name;
		this.c_address = c_address;
		this.c_phone = c_phone;
		this.c_comment = c_comment;
	}
}
class DataClass6 {
	DataClass2 c;
	DataClass3 o;
	DataClass4 l;
	DataClass1 n;
	DataClass6(DataClass2 c, DataClass3 o, DataClass4 l, DataClass1 n) {
		this.c = c;
		this.o = o;
		this.l = l;
		this.n = n;
	}
}
class DataClass7 {
	int c_custkey;
	String c_name;
	double c_acctbal;
	String c_address;
	String c_phone;
	String c_comment;
	String n_name;
	DataClass7(int c_custkey, String c_name, double c_acctbal, String c_address, String c_phone, String c_comment, String n_name) {
		this.c_custkey = c_custkey;
		this.c_name = c_name;
		this.c_acctbal = c_acctbal;
		this.c_address = c_address;
		this.c_phone = c_phone;
		this.c_comment = c_comment;
		this.n_name = n_name;
	}
}
public class Main {
	static List<DataClass1> nation = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "BRAZIL")));
	static List<DataClass2> customer = new ArrayList<>(java.util.Arrays.asList(new DataClass2(1, "Alice", 100.000000, 1, "123 St", "123-456", "Loyal")));
	static List<DataClass3> orders = new ArrayList<>(java.util.Arrays.asList(new DataClass3(1000, 1, "1993-10-15"), new DataClass3(2000, 1, "1994-01-02")));
	static List<DataClass4> lineitem = new ArrayList<>(java.util.Arrays.asList(new DataClass4(1000, "R", 1000.000000, 0.100000), new DataClass4(2000, "N", 500.000000, 0.000000)));
	static String start_date = "1993-10-01";
	static String end_date = "1994-01-01";
	static List<DataClass5> result = (new java.util.function.Supplier<List<DataClass5>>(){public List<DataClass5> get(){
	List<DataClass5> _res6 = new ArrayList<>();
	Map<DataClass7,List<DataClass6>> _groups7 = new LinkedHashMap<>();
	for (var c : customer) {
		for (var o : orders) {
			if (!(Objects.equals(o.o_custkey, c.c_custkey))) continue;
			for (var l : lineitem) {
				if (!(Objects.equals(l.l_orderkey, o.o_orderkey))) continue;
				for (var n : nation) {
					if (!(Objects.equals(n.n_nationkey, c.c_nationkey))) continue;
					if (!(Objects.equals(String.valueOf(String.valueOf(o.o_orderdate).compareTo(String.valueOf(start_date)) >= 0 && o.o_orderdate).compareTo(String.valueOf(end_date)) < 0 && l.l_returnflag, "R"))) continue;
					DataClass6 _row8 = new DataClass6(c, o, l, n);
					DataClass7 _key9 = new DataClass7(c.c_custkey, c.c_name, c.c_acctbal, c.c_address, c.c_phone, c.c_comment, n.n_name);
					List<DataClass6> _b10 = _groups7.get(_key9);
					if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
					_b10.add(_row8);
				}
			}
		}
	}
	for (var __e : _groups7.entrySet()) {
		DataClass7 g_key = __e.getKey();
		List<DataClass6> g = __e.getValue();
		_res6.add(new DataClass5(g_key.c_custkey, g_key.c_name, sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res11 = new ArrayList<>();
	for (var x : g) {
		_res11.add(x.l.l_extendedprice * (1 - x.l.l_discount));
	}
	return _res11;
}}).get()), g_key.c_acctbal, g_key.n_name, g_key.c_address, g_key.c_phone, g_key.c_comment));
	}
	return _res6;
}}).get();
	static int sum(List<? extends Number> v) {
		int s = 0;
		for (Number n : v) s += n.intValue();
		return s;
	}
	public static void main(String[] args) {
	System.out.println(result);
	}
}
