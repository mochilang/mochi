public class Main {

    static String[] search_user(java.util.Map<String,Object> directory, String username) {
        return (Object)(directory.get(username));
    }

    static void main() {
        java.util.Map<String,String> client = new java.util.LinkedHashMap<String, Object>(java.util.Map.of("Base", "dc=example,dc=com", "Host", "ldap.example.com", "Port", 389, "GroupFilter", "(memberUid=%s)"));
        java.util.Map<String,Object> directory = new java.util.LinkedHashMap<String, Object>(java.util.Map.of("username", new String[]{"admins", "users"}, "john", new String[]{"users"}));
        String[] groups = search_user(directory, "username");
        if (groups.length > 0) {
            String out = "Groups: [";
            int i = 0;
            while (i < groups.length) {
                out = out + "\"" + groups[i] + "\"";
                if (i < groups.length - 1) {
                    out = out + ", ";
                }
                i = i + 1;
            }
            out = out + "]";
            System.out.println(out);
        } else {
            System.out.println("User not found");
        }
    }
    public static void main(String[] args) {
        main();
    }
}
