public class Main {
    static String[] dayNames = new String[]{"Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"};
    static String[] seasons = new String[]{"Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"};
    static String[][] holydays = new String[][]{new String[]{"Mungday", "Chaoflux"}, new String[]{"Mojoday", "Discoflux"}, new String[]{"Syaday", "Confuflux"}, new String[]{"Zaraday", "Bureflux"}, new String[]{"Maladay", "Afflux"}};
    static int[] daysBefore = new int[]{0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

    static boolean isLeap(int y) {
        if (Math.floorMod(y, 400) == 0) {
            return true;
        }
        if (Math.floorMod(y, 100) == 0) {
            return false;
        }
        return Math.floorMod(y, 4) == 0;
    }

    static int dayOfYear(int y, int m, int d) {
        int doy = daysBefore[m - 1] + d;
        if (m > 2 && isLeap(y)) {
            doy = doy + 1;
        }
        return doy;
    }

    static String ordinal(int n) {
        String suff = "th";
        int mod100 = Math.floorMod(n, 100);
        if (mod100 < 11 || mod100 > 13) {
            int r = Math.floorMod(n, 10);
            if (r == 1) {
                suff = "st";
            } else             if (r == 2) {
                suff = "nd";
            } else             if (r == 3) {
                suff = "rd";
            }
        }
        return String.valueOf(n) + suff;
    }

    static String discordian(int y, int m, int d) {
        if (isLeap(y) && m == 2 && d == 29) {
            return "St. Tib's Day, YOLD " + String.valueOf(y + 1166);
        }
        int doy = dayOfYear(y, m, d);
        if (isLeap(y) && doy > 60) {
            doy = doy - 1;
        }
        int idx = doy - 1;
        int season = idx / 73;
        int day = Math.floorMod(idx, 73);
        String res = dayNames[Math.floorMod(idx, 5)] + ", the " + String.valueOf(ordinal(day + 1)) + " day of " + seasons[season] + " in the YOLD " + String.valueOf(y + 1166);
        if (day == 4) {
            res = res + ". Celebrate " + holydays[season][0] + "!";
        }
        if (day == 49) {
            res = res + ". Celebrate " + holydays[season][1] + "!";
        }
        return res;
    }

    static void main() {
        int[][] dates = new int[][]{new int[]{2010, 7, 22}, new int[]{2012, 2, 28}, new int[]{2012, 2, 29}, new int[]{2012, 3, 1}, new int[]{2012, 12, 31}, new int[]{2013, 1, 1}, new int[]{2100, 12, 31}, new int[]{2015, 10, 19}, new int[]{2010, 1, 5}, new int[]{2011, 5, 3}, new int[]{2000, 3, 13}};
        int i = 0;
        while (i < dates.length) {
            int[] dt = dates[i];
            System.out.println(discordian(dt[0], dt[1], dt[2]));
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
