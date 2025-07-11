class Alice {
    int alice;
    Alice(int alice) {
        this.alice = alice;
    }
    int size() { return 1; }
}
public class MapAssign {
    public static void main(String[] args) {
    Alice scores = new Alice(1);
    scores.bob = 2;
    System.out.println(scores.bob);
    }
}
