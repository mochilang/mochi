public class Main {
    static String SAMPLE_HTML;
    static java.util.Map<String,String>[] pharmacy_price_list;

    static int find_substring(String s, String sub, int from) {
        int i = from;
        while (i <= _runeLen(s) - _runeLen(sub)) {
            int j = 0;
            while (j < _runeLen(sub) && (_substr(s, i + j, i + j + 1).equals(_substr(sub, j, j + 1)))) {
                j = j + 1;
            }
            if (j == _runeLen(sub)) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static java.util.Map<String,String>[] fetch_pharmacy_and_price_list(String drug_name, String zip_code) {
        if ((drug_name.equals("")) || (zip_code.equals(""))) {
            return null;
        }
        Object results = (java.util.Map<String,String>[])new java.util.Map[]{};
        int pos = 0;
        String block_tag = "<div class=\"grid-x pharmCard\">";
        String name_tag = "<p class=\"list-title\">";
        String price_tag = "<span class=\"price price-large\">";
        while (true) {
            int div_idx = find_substring(SAMPLE_HTML, block_tag, pos);
            if (div_idx < 0) {
                break;
            }
            int name_start = find_substring(SAMPLE_HTML, name_tag, div_idx);
            if (name_start < 0) {
                break;
            }
            name_start = name_start + _runeLen(name_tag);
            int name_end = find_substring(SAMPLE_HTML, "</p>", name_start);
            if (name_end < 0) {
                break;
            }
            String name = _substr(SAMPLE_HTML, name_start, name_end);
            int price_start = find_substring(SAMPLE_HTML, price_tag, name_end);
            if (price_start < 0) {
                break;
            }
            price_start = price_start + _runeLen(price_tag);
            int price_end = find_substring(SAMPLE_HTML, "</span>", price_start);
            if (price_end < 0) {
                break;
            }
            String price = _substr(SAMPLE_HTML, price_start, price_end);
            results = appendObj(results, new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("pharmacy_name", name), java.util.Map.entry("price", price))));
            pos = price_end;
        }
        return results;
    }
    public static void main(String[] args) {
        SAMPLE_HTML = "<div class=\"grid-x pharmCard\"><p class=\"list-title\">Pharmacy A</p><span class=\"price price-large\">$10.00</span></div><div class=\"grid-x pharmCard\"><p class=\"list-title\">Pharmacy B</p><span class=\"price price-large\">$12.50</span></div>";
        pharmacy_price_list = ((java.util.Map<String,String>[])(fetch_pharmacy_and_price_list("aspirin", "30303")));
        if (!(pharmacy_price_list == null)) {
            int i_1 = 0;
            while (i_1 < pharmacy_price_list.length) {
                java.util.Map<String,String> entry = pharmacy_price_list[i_1];
                System.out.println("Pharmacy: " + ((String)(entry).get("pharmacy_name")) + " Price: " + ((String)(entry).get("price")));
                i_1 = i_1 + 1;
            }
        } else {
            System.out.println("No results found");
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
