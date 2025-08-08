public class Main {
    static String BASE_URL;
    static java.util.Map<String,String>[] ANIME_DB;
    static class Data1 {
        String title;
        String url;
        Data1(String title, String url) {
            this.title = title;
            this.url = url;
        }
        Data1() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'url': '%s'}", String.valueOf(title), String.valueOf(url));
        }
    }

    static java.util.Map<String,Object> EPISODE_DB;
    static class Data2 {
        String title;
        String url;
        Data2(String title, String url) {
            this.title = title;
            this.url = url;
        }
        Data2() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'url': '%s'}", String.valueOf(title), String.valueOf(url));
        }
    }

    static class Data3 {
        String title;
        String url;
        Data3(String title, String url) {
            this.title = title;
            this.url = url;
        }
        Data3() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'url': '%s'}", String.valueOf(title), String.valueOf(url));
        }
    }

    static java.util.Map<String,String> EPISODE_EMBED;

    static boolean contains_str(String s, String sub) {
        if (_runeLen(sub) == 0) {
            return true;
        }
        int i = 0;
        while (i + _runeLen(sub) <= _runeLen(s)) {
            if ((_substr(s, i, i + _runeLen(sub)).equals(sub))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static java.util.Map<String,String>[] search_scraper(String anime_name) {
        String term = anime_name.toLowerCase();
        java.util.Map<String,String>[] res = ((java.util.Map<String,String>[])((java.util.Map<String,String>[])new java.util.Map[]{}));
        int i_1 = 0;
        while (i_1 < ANIME_DB.length) {
            java.util.Map<String,String> item = ANIME_DB[i_1];
            if (((Boolean)(contains_str(((String)(item).get("title")).toLowerCase(), term)))) {
                res = ((java.util.Map<String,String>[])(appendObj(res, item)));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static java.util.Map<String,String>[] search_anime_episode_list(String endpoint) {
        if (((Boolean)(EPISODE_DB.containsKey(endpoint)))) {
            return ((Object)(EPISODE_DB).get(endpoint));
        }
        java.util.Map<String,String>[] empty = ((java.util.Map<String,String>[])((java.util.Map<String,String>[])new java.util.Map[]{}));
        return empty;
    }

    static String to_playlist(String embed) {
        return "/playlist/" + embed.substring(7, _runeLen(embed)) + ".m3u8";
    }

    static String[] get_anime_episode(String endpoint) {
        if (((Boolean)(EPISODE_EMBED.containsKey(endpoint)))) {
            String embed = ((String)(EPISODE_EMBED).get(endpoint));
            String play = BASE_URL + embed;
            String download = BASE_URL + String.valueOf(to_playlist(embed));
            return new String[]{play, download};
        }
        String[] empty_1 = ((String[])(new String[]{}));
        return empty_1;
    }

    static void main() {
        java.util.Map<String,String>[] animes = ((java.util.Map<String,String>[])(search_scraper("demon")));
        System.out.println(java.util.Arrays.toString(animes));
        java.util.Map<String,String>[] episodes = ((java.util.Map<String,String>[])(search_anime_episode_list("/anime/kimetsu-no-yaiba")));
        System.out.println(java.util.Arrays.toString(episodes));
        String[] links = ((String[])(get_anime_episode("/watch/kimetsu-no-yaiba/1")));
        System.out.println(java.util.Arrays.toString(links));
    }
    public static void main(String[] args) {
        BASE_URL = "https://ww7.gogoanime2.org";
        ANIME_DB = ((java.util.Map<String,String>[])(new Data1[]{new Data1("Demon Slayer", "/anime/kimetsu-no-yaiba"), new Data1("Naruto", "/anime/naruto")}));
        EPISODE_DB = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("/anime/kimetsu-no-yaiba", new Data2[]{new Data2("Episode 1", "/watch/kimetsu-no-yaiba/1"), new Data2("Episode 2", "/watch/kimetsu-no-yaiba/2")}), java.util.Map.entry("/anime/naruto", new Data3[]{new Data3("Episode 1", "/watch/naruto/1")})))));
        EPISODE_EMBED = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("/watch/kimetsu-no-yaiba/1", "/embed/kimetsu-no-yaiba/1"), java.util.Map.entry("/watch/kimetsu-no-yaiba/2", "/embed/kimetsu-no-yaiba/2"), java.util.Map.entry("/watch/naruto/1", "/embed/naruto/1")))));
        main();
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
