public class Main {
    static class Product {
        String title;
        String link;
        String price;
        String rating;
        String mrp;
        double discount;
        Product(String title, String link, String price, String rating, String mrp, double discount) {
            this.title = title;
            this.link = link;
            this.price = price;
            this.rating = rating;
            this.mrp = mrp;
            this.discount = discount;
        }
        Product() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'link': '%s', 'price': '%s', 'rating': '%s', 'mrp': '%s', 'discount': %s}", String.valueOf(title), String.valueOf(link), String.valueOf(price), String.valueOf(rating), String.valueOf(mrp), String.valueOf(discount));
        }
    }


    static int find_index(String s, String pat, int start) {
        int i = start;
        while (i <= _runeLen(s) - _runeLen(pat)) {
            int j = 0;
            boolean ok = true;
            while (j < _runeLen(pat)) {
                if (!(s.substring(i + j, i + j+1).equals(pat.substring(j, j+1)))) {
                    ok = false;
                    break;
                }
                j = j + 1;
            }
            if (ok) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String slice_between(String s, String start_pat, String end_pat, int from) {
        int a = find_index(s, start_pat, from);
        if (a < 0) {
            return "";
        }
        int b = a + _runeLen(start_pat);
        int c = find_index(s, end_pat, b);
        if (c < 0) {
            return "";
        }
        return _substr(s, b, c);
    }

    static int char_to_digit(String c) {
        if ((c.equals("0"))) {
            return 0;
        }
        if ((c.equals("1"))) {
            return 1;
        }
        if ((c.equals("2"))) {
            return 2;
        }
        if ((c.equals("3"))) {
            return 3;
        }
        if ((c.equals("4"))) {
            return 4;
        }
        if ((c.equals("5"))) {
            return 5;
        }
        if ((c.equals("6"))) {
            return 6;
        }
        if ((c.equals("7"))) {
            return 7;
        }
        if ((c.equals("8"))) {
            return 8;
        }
        return 9;
    }

    static int parse_int(String txt) {
        int n = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(txt)) {
            String c_1 = txt.substring(i_1, i_1+1);
            if ((c_1.compareTo("0") >= 0) && (c_1.compareTo("9") <= 0)) {
                n = n * 10 + char_to_digit(c_1);
            }
            i_1 = i_1 + 1;
        }
        return n;
    }

    static Product parse_product(String block) {
        String href = String.valueOf(slice_between(block, "href=\"", "\"", 0));
        String link = "https://www.amazon.in" + href;
        String title = String.valueOf(slice_between(block, ">", "</a>", find_index(block, "<a", 0)));
        String price = String.valueOf(slice_between(block, "<span class=\"a-offscreen\">", "</span>", 0));
        String rating = String.valueOf(slice_between(block, "<span class=\"a-icon-alt\">", "</span>", 0));
        if (_runeLen(rating) == 0) {
            rating = "Not available";
        }
        String mrp = String.valueOf(slice_between(block, "<span class=\"a-price a-text-price\">", "</span>", 0));
        double disc = 0.0;
        if (_runeLen(mrp) > 0 && _runeLen(price) > 0) {
            int p = parse_int(price);
            int m = parse_int(mrp);
            if (m > 0) {
                disc = ((Number)(((m - p) * 100))).doubleValue() / (((Number)(m)).doubleValue());
            }
        } else {
            mrp = "";
            disc = 0.0;
        }
        return new Product(title, link, price, rating, mrp, disc);
    }

    static Product[] get_amazon_product_data(String product) {
        String html = "<div class=\"s-result-item\" data-component-type=\"s-search-result\"><h2><a href=\"/sample_product\">Sample Product</a></h2><span class=\"a-offscreen\">₹900</span><span class=\"a-icon-alt\">4.3 out of 5 stars</span><span class=\"a-price a-text-price\">₹1000</span></div><div class=\"s-result-item\" data-component-type=\"s-search-result\"><h2><a href=\"/item2\">Another Product</a></h2><span class=\"a-offscreen\">₹500</span><span class=\"a-icon-alt\">3.8 out of 5 stars</span><span class=\"a-price a-text-price\">₹800</span></div>";
        Product[] out = ((Product[])(new Product[]{}));
        int start = 0;
        while (true) {
            int div_start = find_index(html, "<div class=\"s-result-item\"", start);
            if (div_start < 0) {
                break;
            }
            int div_end = find_index(html, "</div>", div_start);
            if (div_end < 0) {
                break;
            }
            String block = _substr(html, div_start, div_end);
            out = ((Product[])(java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(parse_product(block))).toArray(Product[]::new)));
            start = div_end + _runeLen("</div>");
        }
        return out;
    }

    static void main() {
        Product[] products = ((Product[])(get_amazon_product_data("laptop")));
        int i_2 = 0;
        while (i_2 < products.length) {
            Product p_1 = products[i_2];
            System.out.println(p_1.title + " | " + p_1.link + " | " + p_1.price + " | " + p_1.rating + " | " + p_1.mrp + " | " + _p(p_1.discount));
            i_2 = i_2 + 1;
        }
    }
    public static void main(String[] args) {
        main();
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
