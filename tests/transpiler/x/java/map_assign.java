public class Main {
    static Data1 scores = new Data1(1);
    static class Data1 {
        int alice;
        Data1(int alice) {
            this.alice = alice;
        }
    }


    public static void main(String[] args) {
scores["bob"] = 2;
        System.out.println(scores["bob"]);
    }
}
