public class Main {
    static String msg;
    static int shift;
    static int inc;
    static int clicks;
    static int frames;

    public static void main(String[] args) {
        msg = "Hello World! ";
        shift = 0;
        inc = 1;
        clicks = 0;
        frames = 0;
        while (clicks < 5) {
            String line = "";
            int i = 0;
            while (i < _runeLen(msg)) {
                int idx = Math.floorMod((shift + i), _runeLen(msg));
                line = line + msg.substring(idx, idx + 1);
                i = i + 1;
            }
            System.out.println(line);
            shift = Math.floorMod((shift + inc), _runeLen(msg));
            frames = frames + 1;
            if (Math.floorMod(frames, _runeLen(msg)) == 0) {
                inc = _runeLen(msg) - inc;
                clicks = clicks + 1;
            }
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
