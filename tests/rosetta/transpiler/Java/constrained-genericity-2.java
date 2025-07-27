public class Main {
    static class Foodbox {
        Object[] items;
        Foodbox(Object[] items) {
            this.items = items;
        }
        @Override public String toString() {
            return String.format("{'items': %s}", String.valueOf(items));
        }
    }


    public static void main(String[] args) {
    }
}
