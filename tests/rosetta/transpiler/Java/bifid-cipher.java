public class Main {

    static java.util.Map<String,Object> square_to_maps(String[][] square) {
        java.util.Map<String,int[]> emap = new java.util.LinkedHashMap<String, int[]>();
        java.util.Map<String,String> dmap = new java.util.LinkedHashMap<String, String>();
        int x = 0;
        while (x < square.length) {
            String[] row = square[x];
            int y = 0;
            while (y < row.length) {
                String ch = row[y];
emap.put(ch, new int[]{x, y});
dmap.put(String.valueOf(x) + "," + String.valueOf(y), ch);
                y = y + 1;
            }
            x = x + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("e", emap), java.util.Map.entry("d", dmap)));
    }

    static String remove_space(String text, java.util.Map<String,int[]> emap) {
        String s = text.toUpperCase();
        String out = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if (!(ch.equals(" ")) && (emap.containsKey(ch))) {
                out = out + ch;
            }
            i = i + 1;
        }
        return out;
    }

    static String encrypt(String text, java.util.Map<String,int[]> emap, java.util.Map<String,String> dmap) {
        text = String.valueOf(remove_space(text, emap));
        int[] row0 = new int[]{};
        int[] row1 = new int[]{};
        int i = 0;
        while (i < text.length()) {
            String ch = text.substring(i, i + 1);
            int[] xy = (int[])(((int[])emap.get(ch)));
            row0 = java.util.stream.IntStream.concat(java.util.Arrays.stream(row0), java.util.stream.IntStream.of(xy[0])).toArray();
            row1 = java.util.stream.IntStream.concat(java.util.Arrays.stream(row1), java.util.stream.IntStream.of(xy[1])).toArray();
            i = i + 1;
        }
        for (int v : row1) {
            row0 = java.util.stream.IntStream.concat(java.util.Arrays.stream(row0), java.util.stream.IntStream.of(v)).toArray();
        }
        String res = "";
        int j = 0;
        while (j < row0.length) {
            String key = String.valueOf(row0[j]) + "," + String.valueOf(row0[j + 1]);
            res = res + ((String)dmap.get(key));
            j = j + 2;
        }
        return res;
    }

    static String decrypt(String text, java.util.Map<String,int[]> emap, java.util.Map<String,String> dmap) {
        text = String.valueOf(remove_space(text, emap));
        int[] coords = new int[]{};
        int i = 0;
        while (i < text.length()) {
            String ch = text.substring(i, i + 1);
            int[] xy = (int[])(((int[])emap.get(ch)));
            coords = java.util.stream.IntStream.concat(java.util.Arrays.stream(coords), java.util.stream.IntStream.of(xy[0])).toArray();
            coords = java.util.stream.IntStream.concat(java.util.Arrays.stream(coords), java.util.stream.IntStream.of(xy[1])).toArray();
            i = i + 1;
        }
        int half = coords.length / 2;
        int[] k1 = new int[]{};
        int[] k2 = new int[]{};
        int idx = 0;
        while (idx < half) {
            k1 = java.util.stream.IntStream.concat(java.util.Arrays.stream(k1), java.util.stream.IntStream.of(coords[idx])).toArray();
            idx = idx + 1;
        }
        while (idx < coords.length) {
            k2 = java.util.stream.IntStream.concat(java.util.Arrays.stream(k2), java.util.stream.IntStream.of(coords[idx])).toArray();
            idx = idx + 1;
        }
        String res = "";
        int j = 0;
        while (j < half) {
            String key = String.valueOf(k1[j]) + "," + String.valueOf(k2[j]);
            res = res + ((String)dmap.get(key));
            j = j + 1;
        }
        return res;
    }

    static void main() {
        String[][] squareRosetta = new String[][]{new String[]{"A", "B", "C", "D", "E"}, new String[]{"F", "G", "H", "I", "K"}, new String[]{"L", "M", "N", "O", "P"}, new String[]{"Q", "R", "S", "T", "U"}, new String[]{"V", "W", "X", "Y", "Z"}, new String[]{"J", "1", "2", "3", "4"}};
        String[][] squareWikipedia = new String[][]{new String[]{"B", "G", "W", "K", "Z"}, new String[]{"Q", "P", "N", "D", "S"}, new String[]{"I", "O", "A", "X", "E"}, new String[]{"F", "C", "L", "U", "M"}, new String[]{"T", "H", "Y", "V", "R"}, new String[]{"J", "1", "2", "3", "4"}};
        String textRosetta = "0ATTACKATDAWN";
        String textWikipedia = "FLEEATONCE";
        String textTest = "The invasion will start on the first of January";
        java.util.Map<String,Object> maps = square_to_maps(squareRosetta);
        java.util.Map<String,int[]> emap = (java.util.Map<String,int[]>)(((java.util.Map<String,int[]>)maps.get("e")));
        java.util.Map<String,String> dmap = (java.util.Map<String,String>)(((java.util.Map<String,String>)maps.get("d")));
        System.out.println("from Rosettacode");
        System.out.println("original:\t " + textRosetta);
        String s = String.valueOf(encrypt(textRosetta, emap, dmap));
        System.out.println("codiert:\t " + s);
        s = String.valueOf(decrypt(s, emap, dmap));
        System.out.println("and back:\t " + s);
        maps = square_to_maps(squareWikipedia);
        emap = (java.util.Map<String,int[]>)(((java.util.Map<String,int[]>)maps.get("e")));
        dmap = (java.util.Map<String,String>)(((java.util.Map<String,String>)maps.get("d")));
        System.out.println("from Wikipedia");
        System.out.println("original:\t " + textWikipedia);
        s = String.valueOf(encrypt(textWikipedia, emap, dmap));
        System.out.println("codiert:\t " + s);
        s = String.valueOf(decrypt(s, emap, dmap));
        System.out.println("and back:\t " + s);
        maps = square_to_maps(squareWikipedia);
        emap = (java.util.Map<String,int[]>)(((java.util.Map<String,int[]>)maps.get("e")));
        dmap = (java.util.Map<String,String>)(((java.util.Map<String,String>)maps.get("d")));
        System.out.println("from Rosettacode long part");
        System.out.println("original:\t " + textTest);
        s = String.valueOf(encrypt(textTest, emap, dmap));
        System.out.println("codiert:\t " + s);
        s = String.valueOf(decrypt(s, emap, dmap));
        System.out.println("and back:\t " + s);
    }
    public static void main(String[] args) {
        main();
    }
}
