public class Main {
    static Data1[] customers = new Data1[]{new Data1(1, "Alice"), new Data1(2, "Bob"), new Data1(3, "Charlie"), new Data1(4, "Diana")};
    static class Data1 {
        int id;
        String name;
        Data1(int id, String name) {
            this.id = id;
            this.name = name;
        }
        boolean containsKey(String k) {
            if (k.equals("id")) return true;
            if (k.equals("name")) return true;
            return false;
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
        boolean containsKey(String k) {
            if (k.equals("id")) return true;
            if (k.equals("customerId")) return true;
            if (k.equals("total")) return true;
            return false;
        }
    }

    static java.util.List<Result4> result = new java.util.ArrayList<Result4>() {{ java.util.ArrayList<Result4> _tmp = new java.util.ArrayList<>(); for (var c : customers) { for (var o : orders) { if (o.customerId == ((Integer) (c.get("id")))) { _tmp.add(new Result4(((Integer) (c.get("name"))), o)); } } } java.util.ArrayList<Result4> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Result4)list.get(i)); } addAll(_tmp);}};
    static class Result4 {
        String customerName;
        Object order;
        Result4(String customerName, Object order) {
            this.customerName = customerName;
            this.order = order;
        }
        boolean containsKey(String k) {
            if (k.equals("customerName")) return true;
            if (k.equals("order")) return true;
            return false;
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
