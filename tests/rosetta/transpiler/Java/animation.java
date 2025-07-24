public class Main {
    static String msg = "Hello World! ";
    static int shift = 0;
    static int inc = 1;
    static int clicks = 0;
    static int frames = 0;

    public static void main(String[] args) {
        while (clicks < 5) {
            String line = "";
            int i = 0;
            while (i < msg.length()) {
                int idx = (shift + i) % msg.length();
                line = line + msg.substring(idx, idx + 1);
                i = i + 1;
            }
            System.out.println(line);
            shift = (shift + inc) % msg.length();
            frames = frames + 1;
            if (frames % msg.length() == 0) {
                inc = msg.length() - inc;
                clicks = clicks + 1;
            }
        }
    }
}
