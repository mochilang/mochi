public class Main {
    static class EdgeCount {
        int count;
        EdgeCount(int count) {
            this.count = count;
        }
        EdgeCount() {}
        @Override public String toString() {
            return String.format("{'count': %s}", String.valueOf(count));
        }
    }

    static class UserData {
        String username;
        String full_name;
        String biography;
        String business_email;
        String external_url;
        EdgeCount edge_followed_by;
        EdgeCount edge_follow;
        EdgeCount edge_owner_to_timeline_media;
        String profile_pic_url_hd;
        boolean is_verified;
        boolean is_private;
        UserData(String username, String full_name, String biography, String business_email, String external_url, EdgeCount edge_followed_by, EdgeCount edge_follow, EdgeCount edge_owner_to_timeline_media, String profile_pic_url_hd, boolean is_verified, boolean is_private) {
            this.username = username;
            this.full_name = full_name;
            this.biography = biography;
            this.business_email = business_email;
            this.external_url = external_url;
            this.edge_followed_by = edge_followed_by;
            this.edge_follow = edge_follow;
            this.edge_owner_to_timeline_media = edge_owner_to_timeline_media;
            this.profile_pic_url_hd = profile_pic_url_hd;
            this.is_verified = is_verified;
            this.is_private = is_private;
        }
        UserData() {}
        @Override public String toString() {
            return String.format("{'username': '%s', 'full_name': '%s', 'biography': '%s', 'business_email': '%s', 'external_url': '%s', 'edge_followed_by': %s, 'edge_follow': %s, 'edge_owner_to_timeline_media': %s, 'profile_pic_url_hd': '%s', 'is_verified': %s, 'is_private': %s}", String.valueOf(username), String.valueOf(full_name), String.valueOf(biography), String.valueOf(business_email), String.valueOf(external_url), String.valueOf(edge_followed_by), String.valueOf(edge_follow), String.valueOf(edge_owner_to_timeline_media), String.valueOf(profile_pic_url_hd), String.valueOf(is_verified), String.valueOf(is_private));
        }
    }

    static String sample_script;
    static UserData user;

    static int index_of(String s, String sub) {
        int i = 0;
        while (i <= _runeLen(s) - _runeLen(sub)) {
            if ((_substr(s, i, i + _runeLen(sub)).equals(sub))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int parse_int(String s) {
        int value = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            value = value * 10 + (s.substring(i_1, i_1+1));
            i_1 = i_1 + 1;
        }
        return value;
    }

    static String extract_string(String text, String key) {
        String pattern = "\"" + key + "\":\"";
        int start = index_of(text, pattern) + _runeLen(pattern);
        int end = start;
        while (end < _runeLen(text) && !(_substr(text, end, end + 1).equals("\""))) {
            end = end + 1;
        }
        return _substr(text, start, end);
    }

    static int extract_int(String text, String key) {
        String pattern_1 = "\"" + key + "\":{\"count\":";
        int start_1 = index_of(text, pattern_1) + _runeLen(pattern_1);
        int end_1 = start_1;
        while (end_1 < _runeLen(text)) {
            String ch = text.substring(end_1, end_1+1);
            if ((ch.compareTo("0") < 0) || (ch.compareTo("9") > 0)) {
                break;
            }
            end_1 = end_1 + 1;
        }
        String digits = _substr(text, start_1, end_1);
        int num = parse_int(digits);
        return num;
    }

    static boolean extract_bool(String text, String key) {
        String pattern_2 = "\"" + key + "\":";
        int start_2 = index_of(text, pattern_2) + _runeLen(pattern_2);
        String val = _substr(text, start_2, start_2 + 5);
        String first = val.substring(0, 0+1);
        if ((first.equals("t"))) {
            return true;
        }
        return false;
    }

    static UserData extract_user_profile(String script) {
        return new UserData(extract_string(script, "username"), extract_string(script, "full_name"), extract_string(script, "biography"), extract_string(script, "business_email"), extract_string(script, "external_url"), new EdgeCount(extract_int(script, "edge_followed_by")), new EdgeCount(extract_int(script, "edge_follow")), new EdgeCount(extract_int(script, "edge_owner_to_timeline_media")), extract_string(script, "profile_pic_url_hd"), extract_bool(script, "is_verified"), extract_bool(script, "is_private"));
    }
    public static void main(String[] args) {
        sample_script = "{\"entry_data\":{\"ProfilePage\":[{\"graphql\":{\"user\":{\"username\":\"github\",\"full_name\":\"GitHub\",\"biography\":\"Built for developers.\",\"business_email\":\"support@github.com\",\"external_url\":\"https://github.com/readme\",\"edge_followed_by\":{\"count\":120000},\"edge_follow\":{\"count\":16},\"edge_owner_to_timeline_media\":{\"count\":150},\"profile_pic_url_hd\":\"https://instagram.com/pic.jpg\",\"is_verified\":true,\"is_private\":false}}}]}}";
        user = extract_user_profile(sample_script);
        System.out.println(user.full_name + " (" + user.username + ") is " + user.biography);
        System.out.println("number_of_posts = " + _p(user.edge_owner_to_timeline_media.count));
        System.out.println("number_of_followers = " + _p(user.edge_followed_by.count));
        System.out.println("number_of_followings = " + _p(user.edge_follow.count));
        System.out.println("email = " + user.business_email);
        System.out.println("website = " + user.external_url);
        System.out.println("profile_picture_url = " + user.profile_pic_url_hd);
        System.out.println("is_verified = " + _p(user.is_verified));
        System.out.println("is_private = " + _p(user.is_private));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
