public class Main {

    static String receive_file(String[] chunks) {
        String out = "";
        int i = 0;
        System.out.println("File opened");
        System.out.println("Receiving data...");
        while (i < chunks.length) {
            String data = chunks[i];
            if ((data.equals(""))) {
                break;
            }
            out = out + data;
            i = i + 1;
        }
        System.out.println("Successfully received the file");
        System.out.println("Connection closed");
        return out;
    }

    static void main() {
        String[] incoming = ((String[])(new String[]{"Hello ", "from ", "server"}));
        String received = String.valueOf(receive_file(((String[])(incoming))));
        System.out.println(received);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
