public class Main {
    static Data1 data = new Data1(new Data2(1));
    static class Data2 {
        int inner;
        Data2(int inner) {
            this.inner = inner;
        }
    }

    static class Data1 {
         outer;
        Data1( outer) {
            this.outer = outer;
        }
    }


    public static void main(String[] args) {
data["outer"]["inner"] = 2;
        System.out.println(data["outer"]["inner"]);
    }
}
