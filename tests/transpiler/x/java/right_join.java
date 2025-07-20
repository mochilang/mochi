public class Main {
    static Data1[] customers = new Data1[]{new Data1(1, "Alice"), new Data1(2, "Bob"), new Data1(3, "Charlie"), new Data1(4, "Diana")};
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

    static java.util.List<Result4> result = new java.util.ArrayList<Result4>() {{ for (var c : customers) { for (var o : orders) { if (o.customerId == c.id) { add(new Result4(c.name, o)); } } }}};
    static class Data3 {
         customerName;
         order;
        Data3( customerName,  order) {
            this.customerName = customerName;
            this.order = order;
        }
    }

    static class Result4 {
         customerName;
         order;
        Result4( customerName,  order) {
            this.customerName = customerName;
            this.order = order;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Right Join using syntax ---");
        for (var entry : result) {
            if (entry.order) {
                System.out.println("Customer" + " " + entry.customerName + " " + "has order" + " " + entry.order.id + " " + "- $" + " " + entry.order.total);
            } else {
                System.out.println("Customer" + " " + entry.customerName + " " + "has no orders");
            }
        }
    }
}
