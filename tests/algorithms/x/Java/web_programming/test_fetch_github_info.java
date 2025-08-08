public class Main {
    static String AUTHENTICATED_USER_ENDPOINT;
    static class GitHubUser {
        String login;
        int id;
        GitHubUser(String login, int id) {
            this.login = login;
            this.id = id;
        }
        GitHubUser() {}
        @Override public String toString() {
            return String.format("{'login': '%s', 'id': %s}", String.valueOf(login), String.valueOf(id));
        }
    }


    static String get_header(String[][] hs, String key) {
        int i = 0;
        while (i < hs.length) {
            String[] pair = ((String[])(hs[i]));
            if ((pair[0].equals(key))) {
                return pair[1];
            }
            i = i + 1;
        }
        return "";
    }

    static GitHubUser mock_response(String url, String[][] headers) {
        if (!(url.equals(AUTHENTICATED_USER_ENDPOINT))) {
            throw new RuntimeException(String.valueOf("wrong url"));
        }
        String auth = String.valueOf(get_header(((String[][])(headers)), "Authorization"));
        if (_runeLen(auth) == 0) {
            throw new RuntimeException(String.valueOf("missing Authorization"));
        }
        if (!(auth.substring(0, 6).equals("token "))) {
            throw new RuntimeException(String.valueOf("bad token prefix"));
        }
        String accept = String.valueOf(get_header(((String[][])(headers)), "Accept"));
        if (_runeLen(accept) == 0) {
            throw new RuntimeException(String.valueOf("missing Accept"));
        }
        return new GitHubUser("test", 1);
    }

    static GitHubUser fetch_github_info(String auth_token) {
        String[][] headers = ((String[][])(new String[][]{new String[]{"Authorization", "token " + auth_token}, new String[]{"Accept", "application/vnd.github.v3+json"}}));
        return mock_response(AUTHENTICATED_USER_ENDPOINT, ((String[][])(headers)));
    }

    static void test_fetch_github_info() {
        GitHubUser result = fetch_github_info("token");
        if (!(result.login.equals("test"))) {
            throw new RuntimeException(String.valueOf("login mismatch"));
        }
        if (result.id != 1) {
            throw new RuntimeException(String.valueOf("id mismatch"));
        }
        System.out.println(result.login);
        System.out.println(_p(result.id));
    }

    static void main() {
        test_fetch_github_info();
    }
    public static void main(String[] args) {
        AUTHENTICATED_USER_ENDPOINT = "https://api.github.com/user";
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
