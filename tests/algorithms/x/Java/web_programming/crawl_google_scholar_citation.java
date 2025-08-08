public class Main {
    static String DIGITS;

    static boolean is_digit(String ch) {
        int i = 0;
        while (i < _runeLen(DIGITS)) {
            if ((DIGITS.substring(i, i+1).equals(ch))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static int find_substring(String haystack, String needle) {
        int i_1 = 0;
        while (i_1 <= _runeLen(haystack) - _runeLen(needle)) {
            int j = 0;
            while (j < _runeLen(needle)) {
                if (!(haystack.substring(i_1 + j, i_1 + j+1).equals(needle.substring(j, j+1)))) {
                    break;
                }
                j = j + 1;
            }
            if (j == _runeLen(needle)) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static String extract_citation(String html) {
        String marker = "Cited by ";
        int idx = find_substring(html, marker);
        if (idx < 0) {
            return "";
        }
        int pos = idx + _runeLen(marker);
        String result = "";
        while (pos < _runeLen(html)) {
            String ch = html.substring(pos, pos+1);
            if (!(Boolean)is_digit(ch)) {
                break;
            }
            result = result + ch;
            pos = pos + 1;
        }
        return result;
    }

    static String get_citation(String base_url, java.util.Map<String,String> params) {
        String html = "<div class=\"gs_ri\"><div class=\"gs_fl\"><a>Cited by 123</a></div></div>";
        return extract_citation(html);
    }
    public static void main(String[] args) {
        DIGITS = "0123456789";
        if (((Number)(__name__)).intValue() == "__main__") {
            java.util.Map<String,String> params = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("title", "Precisely geometry controlled microsupercapacitors for ultrahigh areal capacitance, volumetric capacitance, and energy density"), java.util.Map.entry("journal", "Chem. Mater."), java.util.Map.entry("volume", "30"), java.util.Map.entry("pages", "3979-3990"), java.util.Map.entry("year", "2018"), java.util.Map.entry("hl", "en")))));
            System.out.println(get_citation("https://scholar.google.com/scholar_lookup", params));
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
