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

    static Data2[] orders = new Data2[]{new Data2(100, 1, 250), new Data2(101, 3, 80)};
    static class Data2 {
        int id;
        int customerId;
        int total;
        Data2(int id, int customerId, int total) {
            this.id = id;
            this.customerId = customerId;
            this.total = total;
        }
    }

    static java.util.List<Result4> result = new java.util.ArrayList<Result4>() {{ for (var o : orders) { for (var c : customers) { if (o.customerId == c.id) { add(new Result4(o.id, c, o.total)); } } }}};
    static class Result4 {
        int orderId;
        Object customer;
        int total;
        Result4(int orderId, Object customer, int total) {
            this.orderId = orderId;
            this.customer = customer;
            this.total = total;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Left Join ---");
        for (var entry : result) {
            System.out.println("Order" + " " + entry.orderId + " " + "customer" + " " + entry.customer + " " + "total" + " " + entry.total);
        }
    }
}
