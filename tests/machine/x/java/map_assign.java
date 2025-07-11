import java.util.*;
class ScoresAlice {
	int alice;
	ScoresAlice(int alice) {
		this.alice = alice;
	}
}
public class Main {
	static ScoresAlice scores = new ScoresAlice(1);
	public static void main(String[] args) {
	scores.bob = 2;
	System.out.println(scores.bob);
	}
}
