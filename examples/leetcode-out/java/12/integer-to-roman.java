public class Main {
	static String intToRoman(int num) {
		int[] values = new int[]{1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};
		String[] symbols = new String[]{"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
		String result = "";
		int i = 0;
		while ((num > 0)) {
			while ((num >= values[i])) {
				result = (result + symbols[i]);
				num = (num - values[i]);
			}
			i = (i + 1);
		}
		return result;
	}
	
	public static void main(String[] args) {
	}
}
