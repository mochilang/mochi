public class Main {
    static Data1[] customers = new Data1[]{new Data1(1, "Alice"), new Data1(2, "Bob")};
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

    static Data2[] orders = new Data2[]{new Data2(100, 1), new Data2(101, 2)};
    static class Data2 {
        int id;
        int customerId;
        Data2(int id, int customerId) {
            this.id = id;
            this.customerId = customerId;
        }
        boolean containsKey(String k) {
            if (k.equals("id")) return true;
            if (k.equals("customerId")) return true;
            return false;
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
        boolean containsKey(String k) {
            if (k.equals("orderId")) return true;
            if (k.equals("sku")) return true;
            return false;
        }
    }

    static java.util.List<Result5> result = new java.util.ArrayList<Result5>() {{ java.util.ArrayList<Result5> _tmp = new java.util.ArrayList<>(); for (var o : orders) { for (var c : customers) { if (((Integer) (o.get("customerId"))) == c.id) { for (var i : items) { if (((Integer) (o.get("id"))) == i.orderId) { _tmp.add(new Result5(((Integer) (o.get("id"))), c.name, i)); } } } } } java.util.ArrayList<Result5> list = _tmp; int skip = 0; int take = -1; for (int i = 0; i < list.size(); i++) { if (i < skip) continue; if (take >= 0 && i >= skip + take) break; _tmp.add((Result5)list.get(i)); } addAll(_tmp);}};
    static class Result5 {
        int orderId;
        Object name;
        Object item;
        Result5(int orderId, Object name, Object item) {
            this.orderId = orderId;
            this.name = name;
            this.item = item;
        }
        boolean containsKey(String k) {
            if (k.equals("orderId")) return true;
            if (k.equals("name")) return true;
            if (k.equals("item")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
        System.out.println("--- Left Join Multi ---");
        for (var r : result) {
            System.out.println(r.orderId + " " + r.name + " " + r.item);
        }
    }
}
