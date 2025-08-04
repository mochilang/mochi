public class Main {
    static java.util.Map<String,Object> house1;
    static java.util.Map<String,Object> house2;
    static java.util.Map<String,Object> h1_bedrooms;
    static java.util.Map<String,Object> h1_bathrooms;
    static java.util.Map<String,Object> h1_attic;
    static java.util.Map<String,Object> h1_kitchen;
    static java.util.Map<String,Object> h1_living_rooms;
    static java.util.Map<String,Object> h1_basement;
    static java.util.Map<String,Object> h1_garage;
    static java.util.Map<String,Object> h1_garden;
    static java.util.Map<String,Object> h2_upstairs;
    static java.util.Map<String,Object> h2_groundfloor;
    static java.util.Map<String,Object> h2_basement;
    static java.util.Map<String,Object> h1_bathroom1;
    static java.util.Map<String,Object> h1_bathroom2;
    static java.util.Map<String,Object> h1_outside;
    static java.util.Map<String,Object> h1_lounge;
    static java.util.Map<String,Object> h1_dining;
    static java.util.Map<String,Object> h1_conservatory;
    static java.util.Map<String,Object> h1_playroom;
    static java.util.Map<String,Object> h2_bedrooms;
    static java.util.Map<String,Object> h2_bathroom;
    static java.util.Map<String,Object> h2_toilet;
    static java.util.Map<String,Object> h2_attics;
    static java.util.Map<String,Object> h2_kitchen;
    static java.util.Map<String,Object> h2_living_rooms;
    static java.util.Map<String,Object> h2_wet_room;
    static java.util.Map<String,Object> h2_garage;
    static java.util.Map<String,Object> h2_garden;
    static java.util.Map<String,Object> h2_hot_tub;
    static java.util.Map<String,Object> h2_cellars;
    static java.util.Map<String,Object> h2_wine_cellar;
    static java.util.Map<String,Object> h2_cinema;
    static java.util.Map<String,Object> h2_suite1;
    static java.util.Map<String,Object> h2_suite2;
    static java.util.Map<String,Object> h2_bedroom3;
    static java.util.Map<String,Object> h2_bedroom4;
    static java.util.Map<String,Object> h2_lounge;
    static java.util.Map<String,Object> h2_dining;
    static java.util.Map<String,Object> h2_conservatory;
    static java.util.Map<String,Object> h2_playroom;

    static double pow10(int n) {
        double r = 1.0;
        int i = 0;
        while (i < n) {
            r = r * 10.0;
            i = i + 1;
        }
        return r;
    }

    static String formatFloat(double f, int prec) {
        double scale = pow10(prec);
        double scaled = (f * scale) + 0.5;
        int n = (((Number)(scaled)).intValue());
        String digits = _p(n);
        while (_runeLen(digits) <= prec) {
            digits = "0" + digits;
        }
        String intPart = _substr(digits, 0, _runeLen(digits) - prec);
        String fracPart = _substr(digits, _runeLen(digits) - prec, _runeLen(digits));
        return intPart + "." + fracPart;
    }

    static String padLeft(String s, int w) {
        String res = "";
        int n_1 = w - _runeLen(s);
        while (n_1 > 0) {
            res = res + " ";
            n_1 = n_1 - 1;
        }
        return res + s;
    }

    static String repeat(String ch, int n) {
        String s = "";
        int i_1 = 0;
        while (i_1 < n) {
            s = s + ch;
            i_1 = i_1 + 1;
        }
        return s;
    }

    static double toFloat(int i) {
        return ((Number)(i)).doubleValue();
    }

    static java.util.Map<String,Object> newNode(String name, int weight, double coverage) {
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("name", name), java.util.Map.entry("weight", weight), java.util.Map.entry("coverage", coverage), java.util.Map.entry("children", new Object[]{})));
    }

    static void addChildren(java.util.Map<String,Object> n, java.util.Map<String,Object>[] nodes) {
        Object cs = (Object)(((Object[])(n).get("children")));
        for (java.util.Map<String,Object> node : nodes) {
            cs = appendObj(cs, node);
        }
n.put("children", cs);
    }

    static void setCoverage(java.util.Map<String,Object> n, double value) {
n.put("coverage", value);
    }

    static double computeCoverage(java.util.Map<String,Object> n) {
        Object[] cs_1 = (Object[])(((Object[])(n).get("children")));
        if (cs_1.length == 0) {
            return ((double)(n).getOrDefault("coverage", 0.0));
        }
        double v1 = 0.0;
        int v2 = 0;
        for (Object node : cs_1) {
            java.util.Map<String,Object> m = ((java.util.Map<String,Object>)(node));
            double c = computeCoverage(m);
            v1 = v1 + toFloat((int)(((int)(m).getOrDefault("weight", 0)))) * c;
            v2 = v2 + (((int)(m).getOrDefault("weight", 0)));
        }
        return v1 / toFloat(v2);
    }

    static String spaces(int n) {
        return _repeat(" ", n);
    }

    static void show(java.util.Map<String,Object> n, int level) {
        int indent = level * 4;
        String name = ((String)(n).get("name"));
        int nl = _runeLen(name) + indent;
        String line = String.valueOf(spaces(indent)) + name;
        line = line + String.valueOf(spaces(32 - nl)) + "|  ";
        line = line + String.valueOf(padLeft(_p(((int)(n).getOrDefault("weight", 0))), 3)) + "   | ";
        line = line + String.valueOf(formatFloat(computeCoverage(n), 6)) + " |";
        System.out.println(line);
        Object[] cs_2 = (Object[])(((Object[])(n).get("children")));
        for (Object child : cs_2) {
            show(((java.util.Map<String,Object>)(child)), level + 1);
        }
    }

    static void main() {
        java.util.Map<String,Object> cleaning = newNode("cleaning", 1, 0.0);
        addChildren(h1_bathrooms, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h1_bathroom1, h1_bathroom2, h1_outside})));
        addChildren(h1_living_rooms, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h1_lounge, h1_dining, h1_conservatory, h1_playroom})));
        addChildren(house1, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h1_bedrooms, h1_bathrooms, h1_attic, h1_kitchen, h1_living_rooms, h1_basement, h1_garage, h1_garden})));
        addChildren(h2_bedrooms, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h2_suite1, h2_suite2, h2_bedroom3, h2_bedroom4})));
        addChildren(h2_upstairs, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h2_bedrooms, h2_bathroom, h2_toilet, h2_attics})));
        addChildren(h2_living_rooms, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h2_lounge, h2_dining, h2_conservatory, h2_playroom})));
        addChildren(h2_groundfloor, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h2_kitchen, h2_living_rooms, h2_wet_room, h2_garage, h2_garden, h2_hot_tub})));
        addChildren(h2_basement, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h2_cellars, h2_wine_cellar, h2_cinema})));
        addChildren(house2, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{h2_upstairs, h2_groundfloor, h2_basement})));
        addChildren(cleaning, ((java.util.Map<String,Object>[])((java.util.Map<String,Object>[])new java.util.Map[]{house1, house2})));
        double topCoverage = computeCoverage(cleaning);
        System.out.println("TOP COVERAGE = " + String.valueOf(formatFloat(topCoverage, 6)));
        System.out.println("");
        System.out.println("NAME HIERARCHY                 | WEIGHT | COVERAGE |");
        show(cleaning, 0);
        setCoverage(h2_cinema, 1.0);
        double diff = computeCoverage(cleaning) - topCoverage;
        System.out.println("");
        System.out.println("If the coverage of the Cinema node were increased from 0.75 to 1");
        System.out.println("the top level coverage would increase by " + String.valueOf(formatFloat(diff, 6)) + " to " + String.valueOf(formatFloat(topCoverage + diff, 6)));
        setCoverage(h2_cinema, 0.75);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            house1 = newNode("house1", 40, 0.0);
            house2 = newNode("house2", 60, 0.0);
            h1_bedrooms = newNode("bedrooms", 1, 0.25);
            h1_bathrooms = newNode("bathrooms", 1, 0.0);
            h1_attic = newNode("attic", 1, 0.75);
            h1_kitchen = newNode("kitchen", 1, 0.1);
            h1_living_rooms = newNode("living_rooms", 1, 0.0);
            h1_basement = newNode("basement", 1, 0.0);
            h1_garage = newNode("garage", 1, 0.0);
            h1_garden = newNode("garden", 1, 0.8);
            h2_upstairs = newNode("upstairs", 1, 0.0);
            h2_groundfloor = newNode("groundfloor", 1, 0.0);
            h2_basement = newNode("basement", 1, 0.0);
            h1_bathroom1 = newNode("bathroom1", 1, 0.5);
            h1_bathroom2 = newNode("bathroom2", 1, 0.0);
            h1_outside = newNode("outside_lavatory", 1, 1.0);
            h1_lounge = newNode("lounge", 1, 0.0);
            h1_dining = newNode("dining_room", 1, 0.0);
            h1_conservatory = newNode("conservatory", 1, 0.0);
            h1_playroom = newNode("playroom", 1, 1.0);
            h2_bedrooms = newNode("bedrooms", 1, 0.0);
            h2_bathroom = newNode("bathroom", 1, 0.0);
            h2_toilet = newNode("toilet", 1, 0.0);
            h2_attics = newNode("attics", 1, 0.6);
            h2_kitchen = newNode("kitchen", 1, 0.0);
            h2_living_rooms = newNode("living_rooms", 1, 0.0);
            h2_wet_room = newNode("wet_room_&_toilet", 1, 0.0);
            h2_garage = newNode("garage", 1, 0.0);
            h2_garden = newNode("garden", 1, 0.9);
            h2_hot_tub = newNode("hot_tub_suite", 1, 1.0);
            h2_cellars = newNode("cellars", 1, 1.0);
            h2_wine_cellar = newNode("wine_cellar", 1, 1.0);
            h2_cinema = newNode("cinema", 1, 0.75);
            h2_suite1 = newNode("suite_1", 1, 0.0);
            h2_suite2 = newNode("suite_2", 1, 0.0);
            h2_bedroom3 = newNode("bedroom_3", 1, 0.0);
            h2_bedroom4 = newNode("bedroom_4", 1, 0.0);
            h2_lounge = newNode("lounge", 1, 0.0);
            h2_dining = newNode("dining_room", 1, 0.0);
            h2_conservatory = newNode("conservatory", 1, 0.0);
            h2_playroom = newNode("playroom", 1, 0.0);
            main();
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
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
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
