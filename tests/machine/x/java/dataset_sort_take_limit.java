import java.util.*;
class DataClass1 {
	String name;
	int price;
	DataClass1(String name, int price) {
		this.name = name;
		this.price = price;
	}
}
public class Main {
	static List<DataClass1> products = new ArrayList<>(java.util.Arrays.asList(new DataClass1("Laptop", 1500), new DataClass1("Smartphone", 900), new DataClass1("Tablet", 600), new DataClass1("Monitor", 300), new DataClass1("Keyboard", 100), new DataClass1("Mouse", 50), new DataClass1("Headphones", 200)));
	static List<Object> expensive = (new java.util.function.Supplier<List<Object>>(){public List<Object> get(){
	List<Object> _res1 = new ArrayList<>();
	for (var p : products) {
		_res1.add(p);
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Top products (excluding most expensive) ---");
	for (Object item : expensive) {
		System.out.println(((Map)item).get("name") + " " + "costs $" + " " + ((Map)item).get("price"));
	}
	}
}
