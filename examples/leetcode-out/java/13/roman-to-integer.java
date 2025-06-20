public class Main {
	static int romanToInt(String s) {
		java.util.Map<String, Integer> values = new java.util.HashMap<>(java.util.Map.of("I", 1, "V", 5, "X", 10, "L", 50, "C", 100, "D", 500, "M", 1000));
		int total = 0;
		int i = 0;
		int n = s.length();
		while ((i < n)) {
			int curr = values.get(_indexString(s, i));
			if (((i + 1) < n)) {
				int next = values.get(_indexString(s, (i + 1)));
				if ((curr < next)) {
					total = ((total + next) - curr);
					i = (i + 2);
					continue;
				}
			}
			total = (total + curr);
			i = (i + 1);
		}
		return total;
	}
	
	public static void main(String[] args) {
	}
	
	static String _indexString(String s, int i) {
		char[] runes = s.toCharArray();
		if (i < 0) i += runes.length;
		if (i < 0 || i >= runes.length) throw new RuntimeException("index out of range");
		return String.valueOf(runes[i]);
	}
}
