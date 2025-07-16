// base64-decode-data.mochi
import java.util.*;

public class Base64DecodeData {
    static String enc = base64Encode(msg);
    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if (Objects.equals(s.charAt(i), ch)) {
                return i;
            }
            i = (int)(i + 1);
        }
        return -1;
    }
    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (str.length() > 0 && Objects.equals(str.charAt(0), "-")) {
            neg = true;
            i = (int)(1);
        }
        int n = 0;
        M0123456789 digits = new M0123456789(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
        while (i < str.length()) {
            n = (int)(n * 10 + ((Number)((Map<?,?>)digits).get(str.charAt(i))).doubleValue());
            i = (int)(i + 1);
        }
        if (neg) {
            n = (int)(-n);
        }
        return n;
    }
    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int idx = indexOf(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = (int)(indexOf(lower, ch));
        if (idx >= 0) {
            return 97 + idx;
        }
        if (String.valueOf(ch).compareTo(String.valueOf("0")) >= 0 && String.valueOf(ch).compareTo(String.valueOf("9")) <= 0) {
            return 48 + parseIntStr(ch);
        }
        if (Objects.equals(ch, "+")) {
            return 43;
        }
        if (Objects.equals(ch, "/")) {
            return 47;
        }
        if (Objects.equals(ch, " ")) {
            return 32;
        }
        if (Objects.equals(ch, "=")) {
            return 61;
        }
        return 0;
    }
    static String chr(int n) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower.substring(n - 97, n - 96);
        }
        if (n >= 48 && n < 58) {
            String digits = "0123456789";
            return digits.substring(n - 48, n - 47);
        }
        if (n == 43) {
            return "+";
        }
        if (n == 47) {
            return "/";
        }
        if (n == 32) {
            return " ";
        }
        if (n == 61) {
            return "=";
        }
        return "?";
    }
    static String toBinary(int n, int bits) {
        String b = "";
        int val = n;
        int i = 0;
        while (i < bits) {
            b = String.valueOf(val % 2) + b;
            val = (int)(Integer.parseInt((val / 2)));
            i = (int)(i + 1);
        }
        return b;
    }
    static int binToInt(String bits) {
        int n = 0;
        int i = 0;
        while (i < bits.length()) {
            n = (int)(n * 2 + parseIntStr(bits.substring(i, i + 1)));
            i = (int)(i + 1);
        }
        return n;
    }
    static String base64Encode(String text) {
        String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        String bin = "";
        for (Number ch : text) {
            bin = bin + toBinary(ord(ch), 8);
        }
        while (!Objects.equals(bin.length() % 6, 0)) {
            bin = bin + "0";
        }
        String out = "";
        int i = 0;
        while (i < bin.length()) {
            String chunk = bin.substring(i, i + 6);
            int val = binToInt(chunk);
            out = out + alphabet.substring(val, val + 1);
            i = (int)(i + 6);
        }
        int pad = (3 - (text.length() % 3)) % 3;
        if (pad == 1) {
            out = out.substring(0, out.length() - 1) + "=";
        }
        if (pad == 2) {
            out = out.substring(0, out.length() - 2) + "==";
        }
        return out;
    }
    static String base64Decode(String enc) {
        String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        String bin = "";
        int i = 0;
        while (i < enc.length()) {
            String ch = enc.charAt(i);
            if (Objects.equals(ch, "=")) {
                break;
            }
            int idx = indexOf(alphabet, ch);
            bin = bin + toBinary(idx, 6);
            i = (int)(i + 1);
        }
        String out = "";
        i = (int)(0);
        while (String.valueOf(i + 8).compareTo(String.valueOf(bin.length())) <= 0) {
            String chunk = bin.substring(i, i + 8);
            int val = binToInt(chunk);
            out = out + chr(val);
            i = (int)(i + 8);
        }
        return out;
    }
    public static void main(String[] args) {
    String msg = "Rosetta Code Base64 decode data task";
    System.out.println("Original : " + msg);
    System.out.println("\nEncoded  : " + enc);
    String dec = base64Decode(enc);
    System.out.println("\nDecoded  : " + dec);
    }
}
