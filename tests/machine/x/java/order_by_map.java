import java.util.*;
class DataAB {
	int a;
	int b;
	DataAB(int a, int b) {
		this.a = a;
		this.b = b;
	}
}
public class OrderByMap {
	static List<DataAB> data = new ArrayList<>(Arrays.asList(new DataAB(1, 2), new DataAB(1, 1), new DataAB(0, 5)));
	static List<DataAB> sorted = (new java.util.function.Supplier<List<DataAB>>(){public List<DataAB> get(){
	List<DataAB> _res1 = new ArrayList<>();
	for (var x : data) {
		_res1.add(x);
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println(sorted);
	}
}
