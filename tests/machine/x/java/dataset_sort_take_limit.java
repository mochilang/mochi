import java.util.*;
class NamePrice {
	String name;
	int price;
	NamePrice(String name, int price) {
		this.name = name;
		this.price = price;
	}
	int size() { return 2; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof NamePrice other)) return false;
		return Objects.equals(this.name, other.name) && Objects.equals(this.price, other.price);
	}
	@Override public int hashCode() {
		return Objects.hash(name, price);
	}
}
public class DatasetSortTakeLimit {
	public static void main(String[] args) {
	List<NamePrice> products = new ArrayList<>(Arrays.asList(new NamePrice("Laptop", 1500), new NamePrice("Smartphone", 900), new NamePrice("Tablet", 600), new NamePrice("Monitor", 300), new NamePrice("Keyboard", 100), new NamePrice("Mouse", 50), new NamePrice("Headphones", 200)));
	List<NamePrice> expensive = (new java.util.function.Supplier<List<NamePrice>>(){public List<NamePrice> get(){
	List<NamePrice> _res0 = new ArrayList<>();
	for (var p : products) {
		_res0.add(p);
	}
	return _res0;
}}).get();
	System.out.println("--- Top products (excluding most expensive) ---");
	for (NamePrice item : expensive) {
		System.out.println(item.name + " " + "costs $" + " " + item.price);
	}
	}
}
