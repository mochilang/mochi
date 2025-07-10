import java.util.*;
class DataClass1 {
	int a;
	int b;
	DataClass1(int a, int b) {
		this.a = a;
		this.b = b;
	}
}
public class Main {
	static List<DataClass1> data = new ArrayList<>(java.util.Arrays.asList(new DataClass1(1, 2), new DataClass1(1, 1), new DataClass1(0, 5)));
	static List<DataClass1> sorted = (new java.util.function.Supplier<List<DataClass1>>(){public List<DataClass1> get(){
	List<DataClass1> _res1 = new ArrayList<>();
	for (var x : data) {
		_res1.add(x);
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println(sorted);
	}
}
