public class Main {
    static String[] values;

    static String decimal_to_hexadecimal(int decimal) {
        int num = decimal;
        boolean negative = false;
        if (num < 0) {
            negative = true;
            num = -num;
        }
        if (num == 0) {
            if (negative) {
                return "-0x0";
            }
            return "0x0";
        }
        String hex = "";
        while (num > 0) {
            int remainder = Math.floorMod(num, 16);
            hex = values[remainder] + hex;
            num = num / 16;
        }
        if (negative) {
            return "-0x" + hex;
        }
        return "0x" + hex;
    }
    public static void main(String[] args) {
        values = ((String[])(new String[]{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"}));
        System.out.println(decimal_to_hexadecimal(5));
        System.out.println(decimal_to_hexadecimal(15));
        System.out.println(decimal_to_hexadecimal(37));
        System.out.println(decimal_to_hexadecimal(255));
        System.out.println(decimal_to_hexadecimal(4096));
        System.out.println(decimal_to_hexadecimal(999098));
        System.out.println(decimal_to_hexadecimal(-256));
        System.out.println(decimal_to_hexadecimal(0));
    }
}
