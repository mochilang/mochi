#include <iostream>
#include <string>
#include <vector>

using namespace std;

static int calculate(const string& expr) {
    int result = 0, number = 0, sign = 1;
    vector<int> stack;
    for (char ch : expr) {
        if (isdigit(static_cast<unsigned char>(ch))) {
            number = number * 10 + (ch - '0');
        } else if (ch == '+' || ch == '-') {
            result += sign * number;
            number = 0;
            sign = ch == '+' ? 1 : -1;
        } else if (ch == '(') {
            stack.push_back(result);
            stack.push_back(sign);
            result = 0;
            number = 0;
            sign = 1;
        } else if (ch == ')') {
            result += sign * number;
            number = 0;
            int prevSign = stack.back();
            stack.pop_back();
            int prevResult = stack.back();
            stack.pop_back();
            result = prevResult + prevSign * result;
        }
    }
    return result + sign * number;
}

int main() {
    string line;
    if (!getline(cin, line)) return 0;
    int t = stoi(line);
    for (int i = 0; i < t; ++i) {
        getline(cin, line);
        if (i) cout << '\n';
        cout << calculate(line);
    }
    return 0;
}
