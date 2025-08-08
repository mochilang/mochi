public class Main {

    static java.util.Map<String,Double> get_imdb_top_250_movies(String url) {
        java.util.Map<String,Double> movies = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("The Shawshank Redemption", 9.2), java.util.Map.entry("The Godfather", 9.2), java.util.Map.entry("The Dark Knight", 9.0)))));
        return movies;
    }

    static void write_movies(String filename) {
        java.util.Map<String,Double> movies_1 = get_imdb_top_250_movies("");
        System.out.println("Movie title,IMDb rating");
        for (String title : movies_1.keySet()) {
            double rating = (double)(((double)(movies_1).getOrDefault(title, 0.0)));
            System.out.println(title + "," + _p(rating));
        }
    }
    public static void main(String[] args) {
        write_movies("IMDb_Top_250_Movies.csv");
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
