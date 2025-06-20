#include <bits/stdc++.h>
using namespace std;

string intToRoman(int num){
	auto values = vector<int>{1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};
	auto symbols = vector<string>{string("M"), string("CM"), string("D"), string("CD"), string("C"), string("XC"), string("L"), string("XL"), string("X"), string("IX"), string("V"), string("IV"), string("I")};
	auto result = string("");
	auto i = 0;
	while (num > 0) {
		while (num >= values[i]) {
			result = result + symbols[i];
			num = num - values[i];
		}
		i = i + 1;
	}
	return result;
}

int main() {
	return 0;
}
