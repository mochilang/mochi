public class Main {
    static Data1 data = new Data1(new Data2(1));
    static class Data2 {
        int inner;
        Data2(int inner) {
            this.inner = inner;
        }
        boolean containsKey(String k) {
            if (k.equals("inner")) return true;
            return false;
        }
    }

    static class Data1 {
         outer;
        Data1( outer) {
            this.outer = outer;
        }
        boolean containsKey(String k) {
            if (k.equals("outer")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
data["outer"]["inner"] = 2;
        System.out.println(data.get("outer")["inner"]);
    }
}
