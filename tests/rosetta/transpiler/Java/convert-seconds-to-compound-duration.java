public class Main {

    static String timeStr(int sec) {
        int wks = sec / 604800;
        sec = Math.floorMod(sec, 604800);
        int ds = sec / 86400;
        sec = Math.floorMod(sec, 86400);
        int hrs = sec / 3600;
        sec = Math.floorMod(sec, 3600);
        int mins = sec / 60;
        sec = Math.floorMod(sec, 60);
        String res = "";
        boolean comma = false;
        if (wks != 0) {
            res = res + String.valueOf(wks) + " wk";
            comma = true;
        }
        if (ds != 0) {
            if (comma) {
                res = res + ", ";
            }
            res = res + String.valueOf(ds) + " d";
            comma = true;
        }
        if (hrs != 0) {
            if (comma) {
                res = res + ", ";
            }
            res = res + String.valueOf(hrs) + " hr";
            comma = true;
        }
        if (mins != 0) {
            if (comma) {
                res = res + ", ";
            }
            res = res + String.valueOf(mins) + " min";
            comma = true;
        }
        if (sec != 0) {
            if (comma) {
                res = res + ", ";
            }
            res = res + String.valueOf(sec) + " sec";
        }
        return res;
    }
    public static void main(String[] args) {
        System.out.println(timeStr(7259));
        System.out.println(timeStr(86400));
        System.out.println(timeStr(6000000));
    }
}
