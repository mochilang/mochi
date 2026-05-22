public class Main {
    static Data1 scores = new Data1(1);
    static class Data1 {
        int alice;
        Data1(int alice) {
            this.alice = alice;
        }
        boolean containsKey(String k) {
            if (k.equals("alice")) return true;
            return false;
        }
    }


    public static void main(String[] args) {
scores.put("bob", 2);
        System.out.println(scores.get("bob"));
    }
}
