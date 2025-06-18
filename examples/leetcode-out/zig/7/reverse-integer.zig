const std = @import("std");

fn reverse(x: i32) [2]i32 {
	var sign = 1;
	var n = x;
	if ((n < 0)) {
		sign = -1;
		n = -n;
	}
	var rev = 0;
	while ((n != 0)) {
		const digit = (n % 10);
		rev = ((rev * 10) + digit);
		n = (n / 10);
	}
	rev = (rev * sign);
	if ((((rev < ((-2147483647 - 1))) or rev) > 2147483647)) {
		return 0;
	}
	return rev;
}

pub fn main() void {
}
