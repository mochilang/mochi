public class Main {
    static Data1[] customers = new Data1[]{new Data1(1, "Alice"), new Data1(2, "Bob"), new Data1(3, "Charlie")};
    static class Data1 {
        int id;
        String name;
        Data1(int id, String name) {
            this.id = id;
            this.name = name;
        }
    }

    static Data2[] orders = new Data2[]{new Data2(100, 1, 250), new Data2(101, 2, 125), new Data2(102, 1, 300)};
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

    static java.util.List<Result4> result = new java.util.ArrayList<Result4>() {{ for (var o : orders) { for (var c : customers) { add(new Result4(o.id, o.customerId, c.name, o.total)); } }}};
    static class Result4 {
        int orderId;
        int orderCustomerId;
        Object pairedCustomerName;
        int orderTotal;
        Result4(int orderId, int orderCustomerId, Object pairedCustomerName, int orderTotal) {
            this.orderId = orderId;
            this.orderCustomerId = orderCustomerId;
            this.pairedCustomerName = pairedCustomerName;
            this.orderTotal = orderTotal;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Cross Join: All order-customer pairs ---");
        for (var entry : result) {
            System.out.println("Order" + " " + entry.orderId + " " + "(customerId:" + " " + entry.orderCustomerId + " " + ", total: $" + " " + entry.orderTotal + " " + ") paired with" + " " + entry.pairedCustomerName);
        }
    }
}
