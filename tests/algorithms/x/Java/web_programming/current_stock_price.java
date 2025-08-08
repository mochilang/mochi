public class Main {

    static int find(String text, String pattern, int start) {
        int i = start;
        int limit = _runeLen(text) - _runeLen(pattern);
        while (i <= limit) {
            if ((_substr(text, i, i + _runeLen(pattern)).equals(pattern))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String stock_price(String symbol) {
        java.util.Map<String,String> pages = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("AAPL", "<span data-testid=\"qsp-price\">228.43</span>"), java.util.Map.entry("AMZN", "<span data-testid=\"qsp-price\">201.85</span>"), java.util.Map.entry("IBM", "<span data-testid=\"qsp-price\">210.30</span>"), java.util.Map.entry("GOOG", "<span data-testid=\"qsp-price\">177.86</span>"), java.util.Map.entry("MSFT", "<span data-testid=\"qsp-price\">414.82</span>"), java.util.Map.entry("ORCL", "<span data-testid=\"qsp-price\">188.87</span>")))));
        if (((Boolean)(pages.containsKey(symbol)))) {
            String html = ((String)(pages).get(symbol));
            String marker = "<span data-testid=\"qsp-price\">";
            int start_idx = find(html, marker, 0);
            if (start_idx != (-1)) {
                int price_start = start_idx + _runeLen(marker);
                int end_idx = find(html, "</span>", price_start);
                if (end_idx != (-1)) {
                    return _substr(html, price_start, end_idx);
                }
            }
        }
        return "No <fin-streamer> tag with the specified data-testid attribute found.";
    }
    public static void main(String[] args) {
        for (String symbol : new String[]{"AAPL", "AMZN", "IBM", "GOOG", "MSFT", "ORCL"}) {
            System.out.println("Current " + symbol + " stock price is " + String.valueOf(stock_price(symbol)));
        }
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
