public class Main {
    static int door = 1;
    static int incrementer = 0;

    public static void main(String[] args) {
        for (int current = 1; current < 101; current++) {
            String line = "Door " + String.valueOf(current) + " ";
            if (current == door) {
                line = line + "Open";
                incrementer = incrementer + 1;
                door = door + 2 * incrementer + 1;
            } else {
                line = line + "Closed";
            }
            System.out.println(line);
        }
    }
}
