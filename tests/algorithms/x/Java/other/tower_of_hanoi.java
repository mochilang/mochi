public class Main {
    static long height = 3L;

    static void move_tower(long height, String from_pole, String to_pole, String with_pole) {
        if ((long)(height) >= 1L) {
            move_tower((long)((long)(height) - 1L), from_pole, with_pole, to_pole);
            move_disk(from_pole, to_pole);
            move_tower((long)((long)(height) - 1L), with_pole, to_pole, from_pole);
        }
    }

    static void move_disk(String fp, String tp) {
        System.out.println("moving disk from " + fp + " to " + tp);
    }
    public static void main(String[] args) {
        move_tower((long)(height), "A", "B", "C");
    }
}
