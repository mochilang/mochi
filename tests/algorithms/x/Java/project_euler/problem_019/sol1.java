public class Main {

    static boolean is_leap(int year) {
        if ((Math.floorMod(year, 4) == 0 && Math.floorMod(year, 100) != 0) || (Math.floorMod(year, 400) == 0)) {
            return true;
        }
        return false;
    }

    static int count_sundays() {
        int[] days_per_month = ((int[])(new int[]{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}));
        int day = 6;
        int month = 1;
        int year = 1901;
        int sundays = 0;
        while (year < 2001) {
            day = day + 7;
            if (((Boolean)(is_leap(year)))) {
                if (day > days_per_month[month - 1] && month != 2) {
                    month = month + 1;
                    day = day - days_per_month[month - 2];
                } else                 if (day > 29 && month == 2) {
                    month = month + 1;
                    day = day - 29;
                }
            } else             if (day > days_per_month[month - 1]) {
                month = month + 1;
                day = day - days_per_month[month - 2];
            }
            if (month > 12) {
                year = year + 1;
                month = 1;
            }
            if (year < 2001 && day == 1) {
                sundays = sundays + 1;
            }
        }
        return sundays;
    }
    public static void main(String[] args) {
        System.out.println(count_sundays());
    }
}
