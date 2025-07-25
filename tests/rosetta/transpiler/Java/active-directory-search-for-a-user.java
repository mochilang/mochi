public class Main {

    static String[] search_user(java.util.Map<String,String[]> directory, String username) {
        return ((String[])directory.get(username));
    }

    static void main() {
        java.util.Map<String,String> client = new java.util.LinkedHashMap<String, String>() {{ put("Base", "dc=example,dc=com"); put("Host", "ldap.example.com"); put("Port", String.valueOf(389)); put("GroupFilter", "(memberUid=%s)"); }};
        java.util.Map<String,String[]> directory = new java.util.LinkedHashMap<String, String[]>() {{ put("username", new String[]{"admins", "users"}); put("john", new String[]{"users"}); }};
        String[] groups = search_user(directory, "username");
        if (groups.length > 0) {
            String out = "Groups: [";
            int i = 0;
            while (i < groups.length) {
                out = String.valueOf(String.valueOf(out + "\"" + groups[i]) + "\"");
                if (i < groups.length - 1) {
                    out = String.valueOf(out + ", ");
                }
                i = i + 1;
            }
            out = String.valueOf(out + "]");
            System.out.println(out);
        } else {
            System.out.println("User not found");
        }
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
