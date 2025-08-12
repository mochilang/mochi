public class Main {

    static long index_of(String s, String ch) {
        long i = 0L;
        while (i < _runeLen(s)) {
            if ((s.substring((int)((long)(i)), (int)((long)(i))+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static long ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        String digits_1 = "0123456789";
        long idx_1 = index_of(upper, ch);
        if (idx_1 >= 0) {
            return 65 + idx_1;
        }
        idx_1 = index_of(lower_1, ch);
        if (idx_1 >= 0) {
            return 97 + idx_1;
        }
        idx_1 = index_of(digits_1, ch);
        if (idx_1 >= 0) {
            return 48 + idx_1;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        return 0;
    }

    static long djb2(String s) {
        long hash_value = 5381L;
        long i_2 = 0L;
        while (i_2 < _runeLen(s)) {
            hash_value = hash_value * 33 + ord(s.substring((int)((long)(i_2)), (int)((long)(i_2))+1));
            hash_value = Math.floorMod(hash_value, 4294967296L);
            i_2 = i_2 + 1;
        }
        return hash_value;
    }
    public static void main(String[] args) {
        System.out.println(djb2("Algorithms"));
        System.out.println(djb2("scramble bits"));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
