#include <iostream>
#include <string>
#include <vector>

using namespace std;

static const vector<string> less20 = {"", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
                                      "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen",
                                      "Seventeen", "Eighteen", "Nineteen"};
static const vector<string> tens = {"", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"};
static const vector<string> thousands = {"", "Thousand", "Million", "Billion"};

static string helper(int n) {
    if (n == 0) return "";
    if (n < 20) return less20[n];
    if (n < 100) return tens[n / 10] + (n % 10 ? " " + helper(n % 10) : "");
    return less20[n / 100] + " Hundred" + (n % 100 ? " " + helper(n % 100) : "");
}

static string solve(int num) {
    if (num == 0) return "Zero";
    vector<string> parts;
    for (int idx = 0; num > 0; ++idx) {
        int chunk = num % 1000;
        if (chunk) {
            string words = helper(chunk);
            if (!thousands[idx].empty()) words += " " + thousands[idx];
            parts.push_back(words);
        }
        num /= 1000;
    }
    string ans;
    for (int i = (int)parts.size() - 1; i >= 0; --i) {
        if (!ans.empty()) ans += " ";
        ans += parts[i];
    }
    return ans;
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int i = 0; i < t; ++i) {
        int num;
        cin >> num;
        if (i) cout << '\n';
        cout << solve(num);
    }
    return 0;
}
