public class Main {
    static class CovidData {
        int cases;
        int deaths;
        int recovered;
        CovidData(int cases, int deaths, int recovered) {
            this.cases = cases;
            this.deaths = deaths;
            this.recovered = recovered;
        }
        CovidData() {}
        @Override public String toString() {
            return String.format("{'cases': %s, 'deaths': %s, 'recovered': %s}", String.valueOf(cases), String.valueOf(deaths), String.valueOf(recovered));
        }
    }


    static int parse_int(String s) {
        int value = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            String ch = _substr(s, i, i + 1);
            if ((ch.equals(","))) {
                i = i + 1;
                continue;
            }
            value = value * 10 + (Integer.parseInt(ch));
            i = i + 1;
        }
        return value;
    }

    static int find(String haystack, String needle, int start) {
        int nlen = _runeLen(needle);
        int i_1 = start;
        while (i_1 <= _runeLen(haystack) - nlen) {
            int j = 0;
            boolean matched = true;
            while (j < nlen) {
                if (!(_substr(haystack, i_1 + j, i_1 + j + 1).equals(_substr(needle, j, j + 1)))) {
                    matched = false;
                    break;
                }
                j = j + 1;
            }
            if (matched) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return 0 - 1;
    }

    static int[] extract_numbers(String html) {
        int[] nums = ((int[])(new int[]{}));
        int pos = 0;
        String start_tag = "<span>";
        String end_tag = "</span>";
        while (true) {
            int s = find(html, start_tag, pos);
            if (s == 0 - 1) {
                break;
            }
            int content_start = s + _runeLen(start_tag);
            int e = find(html, end_tag, content_start);
            if (e == 0 - 1) {
                break;
            }
            String num_str = _substr(html, content_start, e);
            nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(nums), java.util.stream.IntStream.of(parse_int(num_str))).toArray()));
            pos = e + _runeLen(end_tag);
        }
        return nums;
    }

    static CovidData covid_stats(String html) {
        int[] nums_1 = ((int[])(extract_numbers(html)));
        return new CovidData(nums_1[0], nums_1[1], nums_1[2]);
    }

    static void main() {
        String sample_html = "<div class=\"maincounter-number\"><span>123456</span></div>" + "<div class=\"maincounter-number\"><span>7890</span></div>" + "<div class=\"maincounter-number\"><span>101112</span></div>";
        CovidData stats = covid_stats(sample_html);
        System.out.println("Total COVID-19 cases in the world: " + _p(stats.cases));
        System.out.println("Total deaths due to COVID-19 in the world: " + _p(stats.deaths));
        System.out.println("Total COVID-19 patients recovered in the world: " + _p(stats.recovered));
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
