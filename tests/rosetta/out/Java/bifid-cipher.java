// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// bifid-cipher.mochi
import java.util.*;

public class BifidCipher {
    static Map<String,Object> square_to_maps(List<List<String>> square) {
        Map<String,List<Integer>> emap = new LinkedHashMap<String,List<Integer>>();
        Map<String,String> dmap = new LinkedHashMap<String,String>();
        int x = 0;
        while (x < square.size()) {
            List<List<String>> row = square.get(x);
            int y = 0;
            while (y < row.size()) {
                List<List<String>> ch = row.get(y);
                emap.put(ch, Arrays.asList(x, y));
                dmap.put(String.valueOf(x) + "," + String.valueOf(y), ch);
                y = (int)(y + 1);
            }
            x = (int)(x + 1);
        }
        return new ED(emap, dmap);
    }
    static String remove_space(String text, Map<String,List<Integer>> emap) {
        Object s = upper(text);
        String out = "";
        int i = 0;
        while (i < s.size()) {
            Object ch = ((List)s).subList(i, i + 1);
            if (!Objects.equals(ch, " ") && emap.containsKey(ch)) {
                out = out + ch;
            }
            i = (int)(i + 1);
        }
        return out;
    }
    static String encrypt(String text, Map<String,List<Integer>> emap, Map<String,String> dmap) {
        text = remove_space(text, emap);
        List<Integer> row0 = Arrays.asList();
        List<Integer> row1 = Arrays.asList();
        int i = 0;
        while (i < text.length()) {
            String ch = text.substring(i, i + 1);
            Map<String,List<Integer>> xy = emap.get(ch);
            row0.add(xy.get(0));
            row1.add(xy.get(1));
            i = (int)(i + 1);
        }
        for (Integer v : row1) {
            row0.add(v);
        }
        String res = "";
        int j = 0;
        while (j < row0.size()) {
            String key = ((Number)((Number)String.valueOf(row0.get(j))).doubleValue() + ",").doubleValue() + ((Number)String.valueOf(row0.get(j + 1))).doubleValue();
            res = res + ((Number)dmap.get(key)).doubleValue();
            j = (int)(j + 2);
        }
        return res;
    }
    static String decrypt(String text, Map<String,List<Integer>> emap, Map<String,String> dmap) {
        text = remove_space(text, emap);
        List<Integer> coords = Arrays.asList();
        int i = 0;
        while (i < text.length()) {
            String ch = text.substring(i, i + 1);
            Map<String,List<Integer>> xy = emap.get(ch);
            coords.add(xy.get(0));
            coords.add(xy.get(1));
            i = (int)(i + 1);
        }
        double half = coords.size() / 2;
        List<Integer> k1 = Arrays.asList();
        List<Integer> k2 = Arrays.asList();
        int idx = 0;
        while (idx < half) {
            k1.add(coords.get(idx));
            idx = (int)(idx + 1);
        }
        while (idx < coords.size()) {
            k2.add(coords.get(idx));
            idx = (int)(idx + 1);
        }
        String res = "";
        int j = 0;
        while (j < half) {
            String key = ((Number)((Number)String.valueOf(k1.get(j))).doubleValue() + ",").doubleValue() + ((Number)String.valueOf(k2.get(j))).doubleValue();
            res = res + ((Number)dmap.get(key)).doubleValue();
            j = (int)(j + 1);
        }
        return res;
    }
    static void main() {
        List<List<String>> squareRosetta = new ArrayList<>(Arrays.asList(Arrays.asList("A", "B", "C", "D", "E"), Arrays.asList("F", "G", "H", "I", "K"), Arrays.asList("L", "M", "N", "O", "P"), Arrays.asList("Q", "R", "S", "T", "U"), Arrays.asList("V", "W", "X", "Y", "Z"), Arrays.asList("J", "1", "2", "3", "4")));
        List<List<String>> squareWikipedia = new ArrayList<>(Arrays.asList(Arrays.asList("B", "G", "W", "K", "Z"), Arrays.asList("Q", "P", "N", "D", "S"), Arrays.asList("I", "O", "A", "X", "E"), Arrays.asList("F", "C", "L", "U", "M"), Arrays.asList("T", "H", "Y", "V", "R"), Arrays.asList("J", "1", "2", "3", "4")));
        String textRosetta = "0ATTACKATDAWN";
        String textWikipedia = "FLEEATONCE";
        String textTest = "The invasion will start on the first of January";
        Map<String,Object> maps = square_to_maps(squareRosetta);
        Map<String,Object> emap = maps.get("e");
        Map<String,Object> dmap = maps.get("d");
        System.out.println("from Rosettacode");
        System.out.println("original:\t " + textRosetta);
        String s = encrypt(textRosetta, emap, dmap);
        System.out.println("codiert:\t " + s);
        s = decrypt(s, emap, dmap);
        System.out.println("and back:\t " + s);
        maps = square_to_maps(squareWikipedia);
        emap = maps.get("e");
        dmap = maps.get("d");
        System.out.println("from Wikipedia");
        System.out.println("original:\t " + textWikipedia);
        s = encrypt(textWikipedia, emap, dmap);
        System.out.println("codiert:\t " + s);
        s = decrypt(s, emap, dmap);
        System.out.println("and back:\t " + s);
        maps = square_to_maps(squareWikipedia);
        emap = maps.get("e");
        dmap = maps.get("d");
        System.out.println("from Rosettacode long part");
        System.out.println("original:\t " + textTest);
        s = encrypt(textTest, emap, dmap);
        System.out.println("codiert:\t " + s);
        s = decrypt(s, emap, dmap);
        System.out.println("and back:\t " + s);
    }
    public static void main(String[] args) {
        main();
    }
}
