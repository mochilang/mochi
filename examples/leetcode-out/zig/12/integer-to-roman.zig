const std = @import("std");

fn intToRoman(num: i32) []const u8 {
	const values = &[_]i32{@as(i32,@intCast(1000)), @as(i32,@intCast(900)), @as(i32,@intCast(500)), @as(i32,@intCast(400)), @as(i32,@intCast(100)), @as(i32,@intCast(90)), @as(i32,@intCast(50)), @as(i32,@intCast(40)), @as(i32,@intCast(10)), @as(i32,@intCast(9)), @as(i32,@intCast(5)), @as(i32,@intCast(4)), @as(i32,@intCast(1))};
	const symbols = &[_]i32{"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
	var result = "";
	var i = @as(i32,@intCast(0));
	while ((num > @as(i32,@intCast(0)))) {
		while ((num >= values[i])) {
			result = (result + symbols[i]);
			num = (num - values[i]);
		}
		i = (i + @as(i32,@intCast(1)));
	}
	return result;
}

pub fn main() void {
}
