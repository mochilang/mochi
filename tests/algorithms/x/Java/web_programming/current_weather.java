public class Main {
    static String OPENWEATHERMAP_API_KEY;
    static String WEATHERSTACK_API_KEY;
    static String OPENWEATHERMAP_URL_BASE;
    static String WEATHERSTACK_URL_BASE;

    static java.util.Map<String,String> http_get(String url, java.util.Map<String,String> params) {
        if (((Boolean)(params.containsKey("q")))) {
            return new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("location", ((String)(params).get("q"))), java.util.Map.entry("temperature", "20")));
        }
        return new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("location", ((String)(params).get("query"))), java.util.Map.entry("temperature", "20")));
    }

    static java.util.Map<String,java.util.Map<String,String>>[] current_weather(String location) {
        Object weather_data = (java.util.Map<String,java.util.Map<String,String>>[])new java.util.Map[]{};
        if (!(OPENWEATHERMAP_API_KEY.equals(""))) {
            java.util.Map<String,Object> params_openweathermap = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("q", location), java.util.Map.entry("appid", OPENWEATHERMAP_API_KEY)))));
            java.util.Map<String,String> response_openweathermap = http_get(OPENWEATHERMAP_URL_BASE, params_openweathermap);
            weather_data = appendObj(weather_data, new java.util.LinkedHashMap<String, java.util.Map<String,String>>(java.util.Map.ofEntries(java.util.Map.entry("OpenWeatherMap", response_openweathermap))));
        }
        if (!(WEATHERSTACK_API_KEY.equals(""))) {
            java.util.Map<String,Object> params_weatherstack = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("query", location), java.util.Map.entry("access_key", WEATHERSTACK_API_KEY)))));
            java.util.Map<String,String> response_weatherstack = http_get(WEATHERSTACK_URL_BASE, params_weatherstack);
            weather_data = appendObj(weather_data, new java.util.LinkedHashMap<String, java.util.Map<String,String>>(java.util.Map.ofEntries(java.util.Map.entry("Weatherstack", response_weatherstack))));
        }
        if (String.valueOf(weather_data).length() == 0) {
            throw new RuntimeException(String.valueOf("No API keys provided or no valid data returned."));
        }
        return weather_data;
    }

    static void main() {
        java.util.Map<String,java.util.Map<String,String>>[] data = ((java.util.Map<String,java.util.Map<String,String>>[])(current_weather("New York")));
        System.out.println(_p(data));
    }
    public static void main(String[] args) {
        OPENWEATHERMAP_API_KEY = "demo";
        WEATHERSTACK_API_KEY = "";
        OPENWEATHERMAP_URL_BASE = "https://api.openweathermap.org/data/2.5/weather";
        WEATHERSTACK_URL_BASE = "http://api.weatherstack.com/current";
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
