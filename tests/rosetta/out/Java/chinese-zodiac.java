// chinese-zodiac.mochi
import java.util.*;

class Info {
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
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Info other)) return false;
        return Objects.equals(this.animal, other.animal) && Objects.equals(this.yinYang, other.yinYang) && Objects.equals(this.element, other.element) && Objects.equals(this.stemBranch, other.stemBranch) && Objects.equals(this.cycle, other.cycle);
    }
    @Override public int hashCode() {
        return Objects.hash(animal, yinYang, element, stemBranch, cycle);
    }
}
public class ChineseZodiac {
    static List<String> animal = new ArrayList<>(Arrays.asList("Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"));
    static List<String> yinYang = new ArrayList<>(Arrays.asList("Yang", "Yin"));
    static List<String> element = new ArrayList<>(Arrays.asList("Wood", "Fire", "Earth", "Metal", "Water"));
    static Info cz(int yr, List<String> animal, List<String> yinYang, List<String> element, List<String> sc, List<String> bc) {
        int y = yr - 4;
        int stem = y % 10;
        int branch = y % 12;
        int sb = ((Number)sc.get(stem)).doubleValue() + ((Number)bc.get(branch)).doubleValue();
        return new Info(String.valueOf(((List)animal.get(branch))), String.valueOf(((List)yinYang.get(stem % 2))), String.valueOf(((List)element.get(Integer.parseInt((stem / 2))))), sb, y % 60 + 1);
    }
    public static void main(String[] args) {
    List<String> stemChArr = new ArrayList<>(Arrays.asList("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"));
    List<String> branchChArr = new ArrayList<>(Arrays.asList("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"));
    for (Integer yr : Arrays.asList(1935, 1938, 1968, 1972, 1976)) {
        Info r = cz(yr, animal, yinYang, element, stemChArr, branchChArr);
        System.out.println(String.valueOf(yr) + ": " + r.element + " " + r.animal + ", " + r.yinYang + ", Cycle year " + String.valueOf(r.cycle) + " " + r.stemBranch);
    }
    }
}
