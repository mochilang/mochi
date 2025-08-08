public class Main {
    static String[] days;
    static String[] zodiac_names;
    static String[] day_messages;

    static String horoscope(int zodiac_sign, String day) {
        int day_index = 0 - 1;
        int i = 0;
        while (i < days.length) {
            if ((day.equals(days[i]))) {
                day_index = i;
                break;
            }
            i = i + 1;
        }
        int sign_index = zodiac_sign - 1;
        if (day_index == 0 - 1) {
            return "Invalid zodiac sign or day";
        }
        if (sign_index < 0 || sign_index >= zodiac_names.length) {
            return "Invalid zodiac sign or day";
        }
        return zodiac_names[sign_index] + ": " + day_messages[day_index];
    }

    static void main() {
        String result = String.valueOf(horoscope(1, "today"));
        System.out.println(result);
    }
    public static void main(String[] args) {
        days = ((String[])(new String[]{"yesterday", "today", "tomorrow"}));
        zodiac_names = ((String[])(new String[]{"Aries", "Taurus", "Gemini", "Cancer", "Leo", "Virgo", "Libra", "Scorpio", "Sagittarius", "Capricorn", "Aquarius", "Pisces"}));
        day_messages = ((String[])(new String[]{"Reflect on what has passed and learn from it.", "Focus on the present and take decisive action.", "Prepare for future opportunities with optimism."}));
        main();
    }
}
