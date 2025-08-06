public class Main {

    static String rstrip_s(String s) {
        if (_runeLen(s) > 0 && (s.substring(_runeLen(s) - 1, _runeLen(s) - 1+1).equals("s"))) {
            return _substr(s, 0, _runeLen(s) - 1);
        }
        return s;
    }

    static String normalize_alias(String u) {
        if ((u.equals("millimeter"))) {
            return "mm";
        }
        if ((u.equals("centimeter"))) {
            return "cm";
        }
        if ((u.equals("meter"))) {
            return "m";
        }
        if ((u.equals("kilometer"))) {
            return "km";
        }
        if ((u.equals("inch"))) {
            return "in";
        }
        if ((u.equals("inche"))) {
            return "in";
        }
        if ((u.equals("feet"))) {
            return "ft";
        }
        if ((u.equals("foot"))) {
            return "ft";
        }
        if ((u.equals("yard"))) {
            return "yd";
        }
        if ((u.equals("mile"))) {
            return "mi";
        }
        return u;
    }

    static boolean has_unit(String u) {
        return (u.equals("mm")) || (u.equals("cm")) || (u.equals("m")) || (u.equals("km")) || (u.equals("in")) || (u.equals("ft")) || (u.equals("yd")) || (u.equals("mi"));
    }

    static double from_factor(String u) {
        if ((u.equals("mm"))) {
            return 0.001;
        }
        if ((u.equals("cm"))) {
            return 0.01;
        }
        if ((u.equals("m"))) {
            return 1.0;
        }
        if ((u.equals("km"))) {
            return 1000.0;
        }
        if ((u.equals("in"))) {
            return 0.0254;
        }
        if ((u.equals("ft"))) {
            return 0.3048;
        }
        if ((u.equals("yd"))) {
            return 0.9144;
        }
        if ((u.equals("mi"))) {
            return 1609.34;
        }
        return 0.0;
    }

    static double to_factor(String u) {
        if ((u.equals("mm"))) {
            return 1000.0;
        }
        if ((u.equals("cm"))) {
            return 100.0;
        }
        if ((u.equals("m"))) {
            return 1.0;
        }
        if ((u.equals("km"))) {
            return 0.001;
        }
        if ((u.equals("in"))) {
            return 39.3701;
        }
        if ((u.equals("ft"))) {
            return 3.28084;
        }
        if ((u.equals("yd"))) {
            return 1.09361;
        }
        if ((u.equals("mi"))) {
            return 0.000621371;
        }
        return 0.0;
    }

    static double length_conversion(double value, String from_type, String to_type) {
        String new_from = String.valueOf(normalize_alias(String.valueOf(rstrip_s(from_type.toLowerCase()))));
        String new_to = String.valueOf(normalize_alias(String.valueOf(rstrip_s(to_type.toLowerCase()))));
        if (!(Boolean)has_unit(new_from)) {
            throw new RuntimeException(String.valueOf("Invalid 'from_type' value: '" + from_type + "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi"));
        }
        if (!(Boolean)has_unit(new_to)) {
            throw new RuntimeException(String.valueOf("Invalid 'to_type' value: '" + to_type + "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi"));
        }
        return value * from_factor(new_from) * to_factor(new_to);
    }
    public static void main(String[] args) {
        System.out.println(length_conversion(4.0, "METER", "FEET"));
        System.out.println(length_conversion(1.0, "kilometer", "inch"));
        System.out.println(length_conversion(2.0, "feet", "meter"));
        System.out.println(length_conversion(2.0, "centimeter", "millimeter"));
        System.out.println(length_conversion(4.0, "yard", "kilometer"));
        System.out.println(length_conversion(3.0, "foot", "inch"));
        System.out.println(length_conversion(3.0, "mm", "in"));
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
