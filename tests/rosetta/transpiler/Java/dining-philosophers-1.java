public class Main {

    static void main() {
        String[] philosophers = new String[]{"Aristotle", "Kant", "Spinoza", "Marx", "Russell"};
        int hunger = 3;
        System.out.println("table empty");
        for (String p : philosophers) {
            System.out.println(p + " seated");
        }
        int idx = 0;
        while (idx < philosophers.length) {
            String name = philosophers[idx];
            int h = 0;
            while (h < hunger) {
                System.out.println(name + " hungry");
                System.out.println(name + " eating");
                System.out.println(name + " thinking");
                h = h + 1;
            }
            System.out.println(name + " satisfied");
            System.out.println(name + " left the table");
            idx = idx + 1;
        }
        System.out.println("table empty");
    }
    public static void main(String[] args) {
        main();
    }
}
