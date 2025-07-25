public class Main {
    static class LDAPClient {
        String Base;
        String Host;
        int Port;
        boolean UseSSL;
        String BindDN;
        String BindPassword;
        String UserFilter;
        String GroupFilter;
        String[] Attributes;
        LDAPClient(String Base, String Host, int Port, boolean UseSSL, String BindDN, String BindPassword, String UserFilter, String GroupFilter, String[] Attributes) {
            this.Base = Base;
            this.Host = Host;
            this.Port = Port;
            this.UseSSL = UseSSL;
            this.BindDN = BindDN;
            this.BindPassword = BindPassword;
            this.UserFilter = UserFilter;
            this.GroupFilter = GroupFilter;
            this.Attributes = Attributes;
        }
        @Override public String toString() {
            return String.format("{'Base': '%s', 'Host': '%s', 'Port': %s, 'UseSSL': %s, 'BindDN': '%s', 'BindPassword': '%s', 'UserFilter': '%s', 'GroupFilter': '%s', 'Attributes': %s}", String.valueOf(Base), String.valueOf(Host), String.valueOf(Port), String.valueOf(UseSSL), String.valueOf(BindDN), String.valueOf(BindPassword), String.valueOf(UserFilter), String.valueOf(GroupFilter), String.valueOf(Attributes));
        }
    }


    static boolean connect(LDAPClient client) {
        return !(client.Host.equals("")) && client.Port > 0;
    }

    static void main() {
        LDAPClient client = new LDAPClient("dc=example,dc=com", "ldap.example.com", 389, false, "uid=readonlyuser,ou=People,dc=example,dc=com", "readonlypassword", "(uid=%s)", "(memberUid=%s)", new String[]{"givenName", "sn", "mail", "uid"});
        if (connect(client)) {
            System.out.println("Connected to " + client.Host);
        } else {
            System.out.println("Failed to connect");
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
