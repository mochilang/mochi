import java.util.*;
class NationNNameNNationkey {
	int n_nationkey;
	String n_name;
	NationNNameNNationkey(int n_nationkey, String n_name) {
		this.n_nationkey = n_nationkey;
		this.n_name = n_name;
	}
}
class CustomerCAcctbalCAddressCCommentCCustkeyCNameCNationkeyCPhone {
	int c_custkey;
	String c_name;
	double c_acctbal;
	int c_nationkey;
	String c_address;
	String c_phone;
	String c_comment;
	CustomerCAcctbalCAddressCCommentCCustkeyCNameCNationkeyCPhone(int c_custkey, String c_name, double c_acctbal, int c_nationkey, String c_address, String c_phone, String c_comment) {
		this.c_custkey = c_custkey;
		this.c_name = c_name;
		this.c_acctbal = c_acctbal;
		this.c_nationkey = c_nationkey;
		this.c_address = c_address;
		this.c_phone = c_phone;
		this.c_comment = c_comment;
	}
}
class OrdersOCustkeyOOrderdateOOrderkey {
	int o_orderkey;
	int o_custkey;
	String o_orderdate;
	OrdersOCustkeyOOrderdateOOrderkey(int o_orderkey, int o_custkey, String o_orderdate) {
		this.o_orderkey = o_orderkey;
		this.o_custkey = o_custkey;
		this.o_orderdate = o_orderdate;
	}
}
class LineitemLDiscountLExtendedpriceLOrderkeyLReturnflag {
	int l_orderkey;
	String l_returnflag;
	double l_extendedprice;
	double l_discount;
	LineitemLDiscountLExtendedpriceLOrderkeyLReturnflag(int l_orderkey, String l_returnflag, double l_extendedprice, double l_discount) {
		this.l_orderkey = l_orderkey;
		this.l_returnflag = l_returnflag;
		this.l_extendedprice = l_extendedprice;
		this.l_discount = l_discount;
	}
}
class ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNNameRevenue {
	Object c_custkey;
	Object c_name;
	int revenue;
	Object c_acctbal;
	Object n_name;
	Object c_address;
	Object c_phone;
	Object c_comment;
	ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNNameRevenue(Object c_custkey, Object c_name, int revenue, Object c_acctbal, Object n_name, Object c_address, Object c_phone, Object c_comment) {
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
class COLN {
	CustomerCAcctbalCAddressCCommentCCustkeyCNameCNationkeyCPhone c;
	OrdersOCustkeyOOrderdateOOrderkey o;
	LineitemLDiscountLExtendedpriceLOrderkeyLReturnflag l;
	NationNNameNNationkey n;
	COLN(CustomerCAcctbalCAddressCCommentCCustkeyCNameCNationkeyCPhone c, OrdersOCustkeyOOrderdateOOrderkey o, LineitemLDiscountLExtendedpriceLOrderkeyLReturnflag l, NationNNameNNationkey n) {
		this.c = c;
		this.o = o;
		this.l = l;
		this.n = n;
	}
}
class ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNName {
	int c_custkey;
	String c_name;
	double c_acctbal;
	String c_address;
	String c_phone;
	String c_comment;
	String n_name;
	ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNName(int c_custkey, String c_name, double c_acctbal, String c_address, String c_phone, String c_comment, String n_name) {
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
	static List<NationNNameNNationkey> nation = new ArrayList<>(Arrays.asList(new NationNNameNNationkey(1, "BRAZIL")));
	static List<CustomerCAcctbalCAddressCCommentCCustkeyCNameCNationkeyCPhone> customer = new ArrayList<>(Arrays.asList(new CustomerCAcctbalCAddressCCommentCCustkeyCNameCNationkeyCPhone(1, "Alice", 100.000000, 1, "123 St", "123-456", "Loyal")));
	static List<OrdersOCustkeyOOrderdateOOrderkey> orders = new ArrayList<>(Arrays.asList(new OrdersOCustkeyOOrderdateOOrderkey(1000, 1, "1993-10-15"), new OrdersOCustkeyOOrderdateOOrderkey(2000, 1, "1994-01-02")));
	static List<LineitemLDiscountLExtendedpriceLOrderkeyLReturnflag> lineitem = new ArrayList<>(Arrays.asList(new LineitemLDiscountLExtendedpriceLOrderkeyLReturnflag(1000, "R", 1000.000000, 0.100000), new LineitemLDiscountLExtendedpriceLOrderkeyLReturnflag(2000, "N", 500.000000, 0.000000)));
	static String start_date = "1993-10-01";
	static String end_date = "1994-01-01";
	static List<ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNNameRevenue> result = (new java.util.function.Supplier<List<ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNNameRevenue>>(){public List<ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNNameRevenue> get(){
	List<ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNNameRevenue> _res6 = new ArrayList<>();
	Map<ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNName,List<COLN>> _groups7 = new LinkedHashMap<>();
	for (var c : customer) {
		for (var o : orders) {
			if (!(Objects.equals(o.o_custkey, c.c_custkey))) continue;
			for (var l : lineitem) {
				if (!(Objects.equals(l.l_orderkey, o.o_orderkey))) continue;
				for (var n : nation) {
					if (!(Objects.equals(n.n_nationkey, c.c_nationkey))) continue;
					if (!(Objects.equals(String.valueOf(String.valueOf(o.o_orderdate).compareTo(String.valueOf(start_date)) >= 0 && o.o_orderdate).compareTo(String.valueOf(end_date)) < 0 && l.l_returnflag, "R"))) continue;
					COLN _row8 = new COLN(c, o, l, n);
					ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNName _key9 = new ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNName(c.c_custkey, c.c_name, c.c_acctbal, c.c_address, c.c_phone, c.c_comment, n.n_name);
					List<COLN> _b10 = _groups7.get(_key9);
					if (_b10 == null) { _b10 = new ArrayList<>(); _groups7.put(_key9, _b10); }
					_b10.add(_row8);
				}
			}
		}
	}
	for (var __e : _groups7.entrySet()) {
		ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNName g_key = __e.getKey();
		List<COLN> g = __e.getValue();
		_res6.add(new ResultCAcctbalCAddressCCommentCCustkeyCNameCPhoneNNameRevenue(g_key.c_custkey, g_key.c_name, sum((List<Number>)(List<?>)(new java.util.function.Supplier<List<Double>>(){public List<Double> get(){
	List<Double> _res11 = new ArrayList<>();
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
