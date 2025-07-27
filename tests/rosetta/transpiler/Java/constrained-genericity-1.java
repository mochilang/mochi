public class Main {
    static class Eatable {
        Runnable eat;
        Eatable(Runnable eat) {
            this.eat = eat;
        }
        @Override public String toString() {
            return String.format("{'eat': %s}", String.valueOf(eat));
        }
    }


    public static void main(String[] args) {
    }
}
