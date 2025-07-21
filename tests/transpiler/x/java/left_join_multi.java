public class Main {
    static Data1[] customers = new Data1[]{new Data1(1, "Alice"), new Data1(2, "Bob")};
    static class Data1 {
        int id;
        String name;
        Data1(int id, String name) {
            this.id = id;
            this.name = name;
        }
    }

    static Data2[] orders = new Data2[]{new Data2(100, 1), new Data2(101, 2)};
    static class Data2 {
        int id;
        int customerId;
        Data2(int id, int customerId) {
            this.id = id;
            this.customerId = customerId;
        }
    }

    static Data3[] items = new Data3[]{new Data3(100, "a")};
    static class Data3 {
        int orderId;
        String sku;
        Data3(int orderId, String sku) {
            this.orderId = orderId;
            this.sku = sku;
        }
    }

    static java.util.List<Result5> result = new java.util.ArrayList<Result5>() {{ for (var o : orders) { for (var c : customers) { if (o.customerId == c.id) { for (var i : items) { if (o.id == i.orderId) { add(new Result5(o.id, c.name, i)); } } } } }}};
    static class Result5 {
        int orderId;
        Object name;
        Object item;
        Result5(int orderId, Object name, Object item) {
            this.orderId = orderId;
            this.name = name;
            this.item = item;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Left Join Multi ---");
        for (var r : result) {
            System.out.println(r.orderId + " " + r.name + " " + r.item);
        }
    }
}
