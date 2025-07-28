public class Main {
    static double PI = 3.141592653589793;
    static class Airport {
        String name;
        String country;
        String icao;
        double lat;
        double lon;
        Airport(String name, String country, String icao, double lat, double lon) {
            this.name = name;
            this.country = country;
            this.icao = icao;
            this.lat = lat;
            this.lon = lon;
        }
        @Override public String toString() {
            return String.format("{'name': '%s', 'country': '%s', 'icao': '%s', 'lat': %s, 'lon': %s}", String.valueOf(name), String.valueOf(country), String.valueOf(icao), String.valueOf(lat), String.valueOf(lon));
        }
    }

    static Airport[] airports = new Airport[]{new Airport("Koksijde Air Base", "Belgium", "EBFN", 51.090301513671875, 2.652780055999756), new Airport("Ostend-Bruges International Airport", "Belgium", "EBOS", 51.198898315399994, 2.8622200489), new Airport("Kent International Airport", "United Kingdom", "EGMH", 51.342201, 1.34611), new Airport("Calais-Dunkerque Airport", "France", "LFAC", 50.962100982666016, 1.954759955406189), new Airport("Westkapelle heliport", "Belgium", "EBKW", 51.32222366333, 3.2930560112), new Airport("Lympne Airport", "United Kingdom", "EGMK", 51.08, 1.013), new Airport("Ursel Air Base", "Belgium", "EBUL", 51.14419937133789, 3.475559949874878), new Airport("Southend Airport", "United Kingdom", "EGMC", 51.5713996887207, 0.6955559849739075), new Airport("Merville-Calonne Airport", "France", "LFQT", 50.61840057373047, 2.642240047454834), new Airport("Wevelgem Airport", "Belgium", "EBKT", 50.817199707, 3.20472002029), new Airport("Midden-Zeeland Airport", "Netherlands", "EHMZ", 51.5121994019, 3.73111009598), new Airport("Lydd Airport", "United Kingdom", "EGMD", 50.95610046386719, 0.9391670227050781), new Airport("RAF Wattisham", "United Kingdom", "EGUW", 52.1273002625, 0.956264019012), new Airport("Beccles Airport", "United Kingdom", "EGSM", 52.435298919699996, 1.6183300018300002), new Airport("Lille/Marcq-en-Baroeul Airport", "France", "LFQO", 50.687198638916016, 3.0755600929260254), new Airport("Lashenden (Headcorn) Airfield", "United Kingdom", "EGKH", 51.156898, 0.641667), new Airport("Le Touquet-Côte d'Opale Airport", "France", "LFAT", 50.517398834228516, 1.6205899715423584), new Airport("Rochester Airport", "United Kingdom", "EGTO", 51.351898193359375, 0.5033329725265503), new Airport("Lille-Lesquin Airport", "France", "LFQQ", 50.563332, 3.086886), new Airport("Thurrock Airfield", "United Kingdom", "EGMT", 51.537505, 0.367634)};

    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 8) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double cosApprox(double x) {
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n <= 8) {
            double denom = ((Number)(((2 * n - 1) * (2 * n)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double atanApprox(double x) {
        if (x > 1.0) {
            return PI / 2.0 - x / (x * x + 0.28);
        }
        if (x < (-1.0)) {
            return -PI / 2.0 - x / (x * x + 0.28);
        }
        return x / (1.0 + 0.28 * x * x);
    }

    static double atan2Approx(double y, double x) {
        if (x > 0.0) {
            double r = atanApprox(y / x);
            return r;
        }
        if (x < 0.0) {
            if (y >= 0.0) {
                return atanApprox(y / x) + PI;
            }
            return atanApprox(y / x) - PI;
        }
        if (y > 0.0) {
            return PI / 2.0;
        }
        if (y < 0.0) {
            return -PI / 2.0;
        }
        return 0.0;
    }

    static double sqrtApprox(double x) {
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double rad(double x) {
        return x * PI / 180.0;
    }

    static double deg(double x) {
        return x * 180.0 / PI;
    }

    static double distance(double lat1, double lon1, double lat2, double lon2) {
        double phi1 = rad(lat1);
        double phi2 = rad(lat2);
        double dphi = rad(lat2 - lat1);
        double dlambda = rad(lon2 - lon1);
        double sdphi = sinApprox(dphi / 2);
        double sdlambda = sinApprox(dlambda / 2);
        double a = sdphi * sdphi + cosApprox(phi1) * cosApprox(phi2) * sdlambda * sdlambda;
        double c = 2 * atan2Approx(sqrtApprox(a), sqrtApprox(1 - a));
        return (6371.0 / 1.852) * c;
    }

    static double bearing(double lat1, double lon1, double lat2, double lon2) {
        double phi1 = rad(lat1);
        double phi2 = rad(lat2);
        double dl = rad(lon2 - lon1);
        double y = sinApprox(dl) * cosApprox(phi2);
        double x = cosApprox(phi1) * sinApprox(phi2) - sinApprox(phi1) * cosApprox(phi2) * cosApprox(dl);
        double br = deg(atan2Approx(y, x));
        if (br < 0) {
            br = br + 360;
        }
        return br;
    }

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double p = 1.0;
        int i = 0;
        while (i < n) {
            p = p * 10.0;
            i = i + 1;
        }
        return p;
    }

    static double round(double x, int n) {
        double m = pow10(n);
        return floor(x * m + 0.5) / m;
    }

    static Object[][] sortByDistance(Object[][] xs) {
        Object[][] arr = xs;
        int i = 1;
        while (i < arr.length) {
            int j = i;
            while (j > 0 && ((Number)(arr[j - 1][0])).intValue() > ((Number)(arr[j][0])).intValue()) {
                Object[] tmp = arr[j - 1];
arr[j - 1] = arr[j];
arr[j] = tmp;
                j = j - 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static void main() {
        double planeLat = 51.514669;
        double planeLon = 2.198581;
        Object[][] results = new Object[][]{};
        for (Airport ap : airports) {
            double d = distance(planeLat, planeLon, ap.lat, ap.lon);
            double b = bearing(planeLat, planeLon, ap.lat, ap.lon);
            results = appendObj(results, new Object[]{d, b, ap});
        }
        results = sortByDistance(results);
        System.out.println("Distance Bearing ICAO Country               Airport");
        System.out.println("--------------------------------------------------------------");
        int i = 0;
        while (i < results.length) {
            Object[] r = results[i];
            Object ap = r[2];
            Object dist = r[0];
            Object bear = r[1];
            String line = String.valueOf(round(((Number)(dist)).doubleValue(), 1)) + "\t" + String.valueOf(round(((Number)(bear)).doubleValue(), 0)) + "\t" + ((Airport)ap).icao + "\t" + ((Airport)ap).country + " " + ((Airport)ap).name;
            System.out.println(line);
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
}
