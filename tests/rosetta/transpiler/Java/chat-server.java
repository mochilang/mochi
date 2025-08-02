public class Main {

    static String[] removeName(String[] names, String name) {
        String[] out = ((String[])(new String[]{}));
        for (String n : names) {
            if (!(n.equals(name))) {
                out = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(n)).toArray(String[]::new)));
            }
        }
        return out;
    }

    static void main() {
        String[][] clients = new String[1][];
        clients[0] = ((String[])(new String[]{}));
        java.util.function.Consumer<String> broadcast = (msg) -> System.out.println(msg);
        java.util.function.Consumer<String> add = (name) -> {
        clients[0] = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(clients[0]), java.util.stream.Stream.of(name)).toArray(String[]::new)));
        broadcast.accept("+++ \"" + name + "\" connected +++\n");
};
        java.util.function.BiConsumer<String,String> send = (name_1, msg_1) -> broadcast.accept(name_1 + "> " + msg_1 + "\n");
        java.util.function.Consumer<String> remove = (name_2) -> {
        clients[0] = ((String[])(removeName(((String[])(clients[0])), name_2)));
        broadcast.accept("--- \"" + name_2 + "\" disconnected ---\n");
};
        add.accept("Alice");
        add.accept("Bob");
        send.accept("Alice", "Hello Bob!");
        send.accept("Bob", "Hi Alice!");
        remove.accept("Bob");
        remove.accept("Alice");
        broadcast.accept("Server stopping!\n");
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
