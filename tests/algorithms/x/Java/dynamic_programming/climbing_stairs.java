public class Main {

    static int climb_stairs(int number_of_steps) {
        if (number_of_steps <= 0) {
            throw new RuntimeException(String.valueOf("number_of_steps needs to be positive"));
        }
        if (number_of_steps == 1) {
            return 1;
        }
        int previous = 1;
        int current = 1;
        int i = 0;
        while (i < number_of_steps - 1) {
            int next = current + previous;
            previous = current;
            current = next;
            i = i + 1;
        }
        return current;
    }
    public static void main(String[] args) {
        System.out.println(climb_stairs(3));
        System.out.println(climb_stairs(1));
    }
}
