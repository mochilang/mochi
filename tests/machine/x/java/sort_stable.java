import java.util.*;
class DataClass1 {
	int n;
	String v;
	DataClass1(int n, String v) {
		this.n = n;
		this.v = v;
	}
}
public class Main {
	static List<DataClass1> items = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, "a"), new DataClass1(1, "b"), new DataClass1(2, "c")));
	static List<Object> result = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res1 = new ArrayList<>();
	for (var i : items) {
		_res1.add(i.v);
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println(result);
	}
}
