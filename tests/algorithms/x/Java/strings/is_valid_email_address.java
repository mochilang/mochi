public class Main {
    static int MAX_LOCAL_PART_OCTETS;
    static int MAX_DOMAIN_OCTETS;
    static String ASCII_LETTERS;
    static String DIGITS;
    static String LOCAL_EXTRA;
    static String DOMAIN_EXTRA;
    static String[] email_tests;
    static int idx = 0;

    static int count_char(String s, String target) {
        int cnt = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(target))) {
                cnt = cnt + 1;
            }
            i = i + 1;
        }
        return cnt;
    }

    static boolean char_in(String c, String allowed) {
        int i_1 = 0;
        while (i_1 < _runeLen(allowed)) {
            if ((_substr(allowed, i_1, i_1 + 1).equals(c))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static boolean starts_with_char(String s, String c) {
        return _runeLen(s) > 0 && (_substr(s, 0, 1).equals(c));
    }

    static boolean ends_with_char(String s, String c) {
        return _runeLen(s) > 0 && (_substr(s, _runeLen(s) - 1, _runeLen(s)).equals(c));
    }

    static boolean contains_double_dot(String s) {
        if (_runeLen(s) < 2) {
            return false;
        }
        int i_2 = 0;
        while (i_2 < _runeLen(s) - 1) {
            if ((_substr(s, i_2, i_2 + 2).equals(".."))) {
                return true;
            }
            i_2 = i_2 + 1;
        }
        return false;
    }

    static boolean is_valid_email_address(String email) {
        if (count_char(email, "@") != 1) {
            return false;
        }
        int at_idx = 0;
        int i_3 = 0;
        while (i_3 < _runeLen(email)) {
            if ((_substr(email, i_3, i_3 + 1).equals("@"))) {
                at_idx = i_3;
                break;
            }
            i_3 = i_3 + 1;
        }
        String local_part = _substr(email, 0, at_idx);
        String domain = _substr(email, at_idx + 1, _runeLen(email));
        if (_runeLen(local_part) > MAX_LOCAL_PART_OCTETS || _runeLen(domain) > MAX_DOMAIN_OCTETS) {
            return false;
        }
        int i_4 = 0;
        while (i_4 < _runeLen(local_part)) {
            String ch = _substr(local_part, i_4, i_4 + 1);
            if (!(Boolean)char_in(ch, ASCII_LETTERS + DIGITS + LOCAL_EXTRA)) {
                return false;
            }
            i_4 = i_4 + 1;
        }
        if (((Boolean)(starts_with_char(local_part, "."))) || ((Boolean)(ends_with_char(local_part, "."))) || ((Boolean)(contains_double_dot(local_part)))) {
            return false;
        }
        i_4 = 0;
        while (i_4 < _runeLen(domain)) {
            String ch_1 = _substr(domain, i_4, i_4 + 1);
            if (!(Boolean)char_in(ch_1, ASCII_LETTERS + DIGITS + DOMAIN_EXTRA)) {
                return false;
            }
            i_4 = i_4 + 1;
        }
        if (((Boolean)(starts_with_char(domain, "-"))) || ((Boolean)(ends_with_char(domain, ".")))) {
            return false;
        }
        if (((Boolean)(starts_with_char(domain, "."))) || ((Boolean)(ends_with_char(domain, "."))) || ((Boolean)(contains_double_dot(domain)))) {
            return false;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            MAX_LOCAL_PART_OCTETS = 64;
            MAX_DOMAIN_OCTETS = 255;
            ASCII_LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
            DIGITS = "0123456789";
            LOCAL_EXTRA = ".(!#$%&'*+-/=?^_`{|}~)";
            DOMAIN_EXTRA = ".-";
            email_tests = ((String[])(new String[]{"simple@example.com", "very.common@example.com", "disposable.style.email.with+symbol@example.com", "other-email-with-hyphen@and.subdomains.example.com", "fully-qualified-domain@example.com", "user.name+tag+sorting@example.com", "x@example.com", "example-indeed@strange-example.com", "test/test@test.com", "123456789012345678901234567890123456789012345678901234567890123@example.com", "admin@mailserver1", "example@s.example", "Abc.example.com", "A@b@c@example.com", "abc@example..com", "a(c)d,e:f;g<h>i[j\\k]l@example.com", "12345678901234567890123456789012345678901234567890123456789012345@example.com", "i.like.underscores@but_its_not_allowed_in_this_part", ""}));
            idx = 0;
            while (idx < email_tests.length) {
                String email = email_tests[idx];
                System.out.println(_p(is_valid_email_address(email)));
                idx = idx + 1;
            }
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
