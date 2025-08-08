public class Main {
    static class Job {
        String title;
        String company;
        Job(String title, String company) {
            this.title = title;
            this.company = company;
        }
        Job() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'company': '%s'}", String.valueOf(title), String.valueOf(company));
        }
    }

    static String SAMPLE_HTML;

    static int indexOf(String s, String sub, int start) {
        int n = _runeLen(s);
        int m = _runeLen(sub);
        int i = start;
        while (i <= n - m) {
            int j = 0;
            boolean ok = true;
            while (j < m) {
                if (!(_substr(s, i + j, i + j + 1).equals(_substr(sub, j, j + 1)))) {
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
        return (0 - 1);
    }

    static String[] splitBy(String s, String sep) {
        String[] res = ((String[])(new String[]{}));
        int start = 0;
        int sepLen = _runeLen(sep);
        int idx = indexOf(s, sep, 0);
        while (idx != (0 - 1)) {
            res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(_substr(s, start, idx))).toArray(String[]::new)));
            start = idx + sepLen;
            idx = indexOf(s, sep, start);
        }
        res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(_substr(s, start, _runeLen(s)))).toArray(String[]::new)));
        return res;
    }

    static String between(String s, String startTag, String endTag) {
        int startIdx = indexOf(s, startTag, 0);
        if (startIdx == (0 - 1)) {
            return "";
        }
        startIdx = startIdx + _runeLen(startTag);
        int endIdx = indexOf(s, endTag, startIdx);
        if (endIdx == (0 - 1)) {
            return "";
        }
        return _substr(s, startIdx, endIdx);
    }

    static String intToString(int n) {
        if (n == 0) {
            return "0";
        }
        int num = n;
        String digits = "";
        while (num > 0) {
            int d = Math.floorMod(num, 10);
            digits = _substr("0123456789", d, d + 1) + digits;
            num = Math.floorDiv(num, 10);
        }
        return digits;
    }

    static Job[] fetch_jobs(String location) {
        String[] blocks = ((String[])(splitBy(SAMPLE_HTML, "data-tn-component=\"organicJob\"")));
        Job[] jobs = ((Job[])(new Job[]{}));
        int i_1 = 1;
        while (i_1 < blocks.length) {
            String block = blocks[i_1];
            String title = String.valueOf(between(block, "data-tn-element=\"jobTitle\">", "</a>"));
            String company = String.valueOf(between(block, "class=\"company\">", "</span>"));
            jobs = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs), java.util.stream.Stream.of(new Job(title, company))).toArray(Job[]::new)));
            i_1 = i_1 + 1;
        }
        return jobs;
    }

    static void main() {
        Job[] jobs_1 = ((Job[])(fetch_jobs("Bangalore")));
        int i_2 = 0;
        while (i_2 < jobs_1.length) {
            Job j_1 = jobs_1[i_2];
            int idx_1 = i_2 + 1;
            System.out.println("Job " + String.valueOf(intToString(idx_1)) + " is " + j_1.title + " at " + j_1.company);
            i_2 = i_2 + 1;
        }
    }
    public static void main(String[] args) {
        SAMPLE_HTML = "<div data-tn-component=\"organicJob\"><a data-tn-element=\"jobTitle\">Android Developer</a><span class=\"company\">ABC Corp</span></div><div data-tn-component=\"organicJob\"><a data-tn-element=\"jobTitle\">iOS Engineer</a><span class=\"company\">XYZ Ltd</span></div>";
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
}
