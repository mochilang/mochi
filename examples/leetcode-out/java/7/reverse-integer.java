class Main {
	static int reverse(int x) {
		var sign = 1;
		var n = x;
		if ((n < 0)) {
			sign = (-1);
			n = (-n);
		}
		var rev = 0;
		while ((n != 0)) {
			var digit = (n % 10);
			rev = ((rev * 10) + digit);
			n = (n / 10);
		}
		rev = (rev * sign);
		if (((rev < ((-2147483647) - 1)) || (rev > 2147483647))) {
			return 0;
		}
		return rev;
	}
	
	public static void main(String[] args) {
	}
}
