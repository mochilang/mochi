public class Main {
    static String[] animal = new String[]{"Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"};
    static String[] yinYang = new String[]{"Yang", "Yin"};
    static String[] element = new String[]{"Wood", "Fire", "Earth", "Metal", "Water"};
    static String[] stemChArr = new String[]{"甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"};
    static String[] branchChArr = new String[]{"子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"};
    static class Info {
        String animal;
        String yinYang;
        String element;
        String stemBranch;
        int cycle;
        Info(String animal, String yinYang, String element, String stemBranch, int cycle) {
            this.animal = animal;
            this.yinYang = yinYang;
            this.element = element;
            this.stemBranch = stemBranch;
            this.cycle = cycle;
        }
        @Override public String toString() {
            return String.format("{'animal': '%s', 'yinYang': '%s', 'element': '%s', 'stemBranch': '%s', 'cycle': %s}", String.valueOf(animal), String.valueOf(yinYang), String.valueOf(element), String.valueOf(stemBranch), String.valueOf(cycle));
        }
    }


    static Info cz(int yr, String[] animal, String[] yinYang, String[] element, String[] sc, String[] bc) {
        int y = yr - 4;
        int stem = Math.floorMod(y, 10);
        int branch = Math.floorMod(y, 12);
        String sb = sc[stem] + bc[branch];
        return new Info(animal[branch], yinYang[Math.floorMod(stem, 2)], element[((Number)((stem / 2))).intValue()], sb, Math.floorMod(y, 60) + 1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (int yr : new int[]{1935, 1938, 1968, 1972, 1976}) {
                Info r = cz(yr, animal, yinYang, element, stemChArr, branchChArr);
                System.out.println(String.valueOf(yr) + ": " + r.element + " " + r.animal + ", " + r.yinYang + ", Cycle year " + String.valueOf(r.cycle) + " " + r.stemBranch);
            }
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
