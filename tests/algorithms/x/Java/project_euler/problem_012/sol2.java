public class Main {

    static int countDivisors(int n) {
        int num = n;
        int total = 1;
        int i = 2;
        while (i * i <= num) {
            int multiplicity = 0;
            while (Math.floorMod(num, i) == 0) {
                num = Math.floorDiv(num, i);
                multiplicity = multiplicity + 1;
            }
            total = total * (multiplicity + 1);
            i = i + 1;
        }
        if (num > 1) {
            total = total * 2;
        }
        return total;
    }

    static int solution() {
        int n = 1;
        int tri = 1;
        while (countDivisors(tri) <= 500) {
            n = n + 1;
            tri = tri + n;
        }
        return tri;
    }
    public static void main(String[] args) {
        System.out.println(solution());
    }
}
