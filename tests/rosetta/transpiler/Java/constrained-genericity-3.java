public class Main {
    static class PeelFirst {
        String value;
        PeelFirst(String value) {
            this.value = value;
        }
        void eat() {
            System.out.println("mm, that " + (String)(value) + " was good!");
        }
        @Override public String toString() {
            return String.format("{'value': '%s'}", String.valueOf(value));
        }
    }


    public static void main(String[] args) {
    }
}
