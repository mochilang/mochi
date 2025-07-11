import java.util.*;
class ProductsNamePrice {
	String name;
	int price;
	ProductsNamePrice(String name, int price) {
		this.name = name;
		this.price = price;
	}
}
public class Main {
	static List<ProductsNamePrice> products = new ArrayList<>(Arrays.asList(new ProductsNamePrice("Laptop", 1500), new ProductsNamePrice("Smartphone", 900), new ProductsNamePrice("Tablet", 600), new ProductsNamePrice("Monitor", 300), new ProductsNamePrice("Keyboard", 100), new ProductsNamePrice("Mouse", 50), new ProductsNamePrice("Headphones", 200)));
	static List<ProductsNamePrice> expensive = (new java.util.function.Supplier<List<ProductsNamePrice>>(){public List<ProductsNamePrice> get(){
	List<ProductsNamePrice> _res1 = new ArrayList<>();
	for (var p : products) {
		_res1.add(p);
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Top products (excluding most expensive) ---");
	for (ProductsNamePrice item : expensive) {
		System.out.println(item.name + " " + "costs $" + " " + item.price);
	}
	}
}
