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

    static Data2[] orders = new Data2[]{new Data2(100, 1, 250), new Data2(101, 2, 125), new Data2(102, 1, 300), new Data2(103, 4, 80)};
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

    static java.util.List<Result4> result = new java.util.ArrayList<Result4>() {{ for (var o : orders) { for (var c : customers) { if (o.customerId == c.id) { add(new Result4(o.id, c.name, o.total)); } } }}};
    static class Data3 {
         orderId;
         customerName;
         total;
        Data3( orderId,  customerName,  total) {
            this.orderId = orderId;
            this.customerName = customerName;
            this.total = total;
        }
    }

    static class Result4 {
         orderId;
         customerName;
         total;
        Result4( orderId,  customerName,  total) {
            this.orderId = orderId;
            this.customerName = customerName;
            this.total = total;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Orders with customer info ---");
        for (var entry : result) {
            System.out.println("Order" + " " + entry.orderId + " " + "by" + " " + entry.customerName + " " + "- $" + " " + entry.total);
        }
    }
}
