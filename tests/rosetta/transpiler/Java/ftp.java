public class Main {
    static class FileInfo {
        String name;
        int size;
        String kind;
        FileInfo(String name, int size, String kind) {
            this.name = name;
            this.size = size;
            this.kind = kind;
        }
        @Override public String toString() {
            return String.format("{'name': '%s', 'size': %s, 'kind': '%s'}", String.valueOf(name), String.valueOf(size), String.valueOf(kind));
        }
    }

    static class FTPConn {
        String dir;
        FTPConn(String dir) {
            this.dir = dir;
        }
        @Override public String toString() {
            return String.format("{'dir': '%s'}", String.valueOf(dir));
        }
    }

    static java.util.Map<String,java.util.Map<String, String>> serverData;
    static java.util.Map<String,String[]> serverNames;

    static FTPConn connect(String hostport) {
        System.out.println("Connected to " + hostport);
        return new FTPConn("/");
    }

    static void login(FTPConn conn, String user, String pass) {
        System.out.println("Logged in as " + user);
    }

    static void changeDir(FTPConn conn, String dir) {
conn.dir = dir;
    }

    static FileInfo[] list(FTPConn conn) {
        String[] names = (String[])(((String[])(serverNames).get(conn.dir)));
        java.util.Map<String, String> dataDir = (java.util.Map<String, String>)(((java.util.Map<String, String>)(serverData).get(conn.dir)));
        FileInfo[] out = ((FileInfo[])(new FileInfo[]{}));
        for (String name : names) {
            String content = ((String)(dataDir).get(name));
            out = ((FileInfo[])(java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(new FileInfo(name, _runeLen(content), "file"))).toArray(FileInfo[]::new)));
        }
        return out;
    }

    static String retrieve(FTPConn conn, String name) {
        return ((String)(((java.util.Map<String, String>)(serverData).get(conn.dir))).get(name));
    }

    static void main() {
        FTPConn conn = connect("localhost:21");
        login(conn, "anonymous", "anonymous");
        changeDir(conn, "pub");
        System.out.println(conn.dir);
        FileInfo[] files = ((FileInfo[])(list(conn)));
        for (FileInfo f : files) {
            System.out.println(f.name + " " + _p(f.size));
        }
        String data = String.valueOf(retrieve(conn, "somefile.bin"));
        System.out.println("Wrote " + _p(_runeLen(data)) + " bytes to somefile.bin");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            serverData = ((java.util.Map<String,java.util.Map<String, String>>)(new java.util.LinkedHashMap<String, java.util.Map<String, String>>(java.util.Map.ofEntries(java.util.Map.entry("pub", ((java.util.Map<String, String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("somefile.bin", "This is a file from the FTP server."), java.util.Map.entry("readme.txt", "Hello from ftp."))))))))));
            serverNames = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry("pub", ((String[])(new String[]{"somefile.bin", "readme.txt"})))))));
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
