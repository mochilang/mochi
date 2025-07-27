public class Main {
    static boolean lockExists = false;

    static void startOnce() {
        if (lockExists) {
            System.out.println("an instance is already running");
        } else {
            lockExists = true;
            System.out.println("single instance started");
        }
    }

    static void main() {
        startOnce();
        startOnce();
    }
    public static void main(String[] args) {
        main();
    }
}
