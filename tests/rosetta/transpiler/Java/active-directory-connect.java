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
        main();
    }
}
