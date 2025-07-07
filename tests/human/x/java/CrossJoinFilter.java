import java.util.*;

class Pair { int n; String l; Pair(int n, String l){this.n=n; this.l=l;} }

public class CrossJoinFilter {
    public static void main(String[] args) {
        int[] nums = {1,2,3};
        String[] letters = {"A","B"};
        List<Pair> pairs = new ArrayList<>();
        for(int n: nums){
            for(String l: letters){
                if(n % 2 == 0){
                    pairs.add(new Pair(n,l));
                }
            }
        }
        System.out.println("--- Even pairs ---");
        for(Pair p : pairs){
            System.out.println(p.n + " " + p.l);
        }
    }
}
