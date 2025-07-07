import java.util.*;

class Product {
    String name;
    int price;
    Product(String name, int price) { this.name = name; this.price = price; }
}

public class DatasetSortTakeLimit {
    public static void main(String[] args) {
        List<Product> products = Arrays.asList(
            new Product("Laptop", 1500),
            new Product("Smartphone", 900),
            new Product("Tablet", 600),
            new Product("Monitor", 300),
            new Product("Keyboard", 100),
            new Product("Mouse", 50),
            new Product("Headphones", 200)
        );

        List<Product> sorted = new ArrayList<>(products);
        sorted.sort((a, b) -> Integer.compare(b.price, a.price));

        List<Product> expensive = sorted.stream().skip(1).limit(3).toList();

        System.out.println("--- Top products (excluding most expensive) ---");
        for (Product p : expensive) {
            System.out.println(p.name + " costs $" + p.price);
        }
    }
}
