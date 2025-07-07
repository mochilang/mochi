import java.util.*;

class Triple { int n; String l; boolean b; Triple(int n,String l,boolean b){this.n=n;this.l=l;this.b=b;} }

public class CrossJoinTriple {
    public static void main(String[] args) {
        int[] nums = {1,2};
        String[] letters = {"A","B"};
        boolean[] bools = {true,false};
        List<Triple> combos = new ArrayList<>();
        for(int n: nums){
            for(String l: letters){
                for(boolean b: bools){
                    combos.add(new Triple(n,l,b));
                }
            }
        }
        System.out.println("--- Cross Join of three lists ---");
        for(Triple c : combos){
            System.out.println(c.n + " " + c.l + " " + c.b);
        }
    }
}
