public class Main {
    static Data1[] products = new Data1[]{new Data1("Laptop", 1500), new Data1("Smartphone", 900), new Data1("Tablet", 600), new Data1("Monitor", 300), new Data1("Keyboard", 100), new Data1("Mouse", 50), new Data1("Headphones", 200)};
    static class Data1 {
        String name;
        int price;
        Data1(String name, int price) {
            this.name = name;
            this.price = price;
        }
        boolean containsKey(String k) {
            if (k.equals("name")) return true;
            if (k.equals("price")) return true;
            return false;
        }
    }

    static java.util.List<Data1> expensive = new java.util.ArrayList<Data1>() {{ java.util.ArrayList<Data1> _tmp = new java.util.ArrayList<>(); for (var p : products) { _tmp.add(p); } java.util.ArrayList<Data1> list = _tmp; list.sort((a, b) -> {Comparable _va = (Comparable)(a.price); Comparable _vb = (Comparable)(b.price); return _vb.compareTo(_va);}); int skip = 1; int take = 3; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Data1)list.get(i)); } addAll(_tmp);}};

    public static void main(String[] args) {
        System.out.println("--- Top products (excluding most expensive) ---");
        for (var item : expensive) {
            System.out.println(item.name + " " + "costs $" + " " + item.price);
        }
    }
}
