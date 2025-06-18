class Main {
	static int[] addTwoNumbers(int[] l1, int[] l2) {
		var i = 0;
		var j = 0;
		var carry = 0;
		var result = new int[]{};
		while ((((i < l1.length) || (j < l2.length)) || (carry > 0))) {
			var x = 0;
			if ((i < l1.length)) {
				x = l1[i];
				i = (i + 1);
			}
			var y = 0;
			if ((j < l2.length)) {
				y = l2[j];
				j = (j + 1);
			}
			var sum = ((x + y) + carry);
			var digit = (sum % 10);
			carry = (sum / 10);
			result = _concat(result, new int[]{digit});
		}
		return result;
	}
	
	public static void main(String[] args) {
	}
	
	static int[] _concat(int[] a, int[] b) {
		int[] res = new int[a.length + b.length];
		System.arraycopy(a, 0, res, 0, a.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
	}
}
