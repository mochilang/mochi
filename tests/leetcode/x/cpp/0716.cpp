#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

struct MaxStack {
    vector<int> stack;
    void push(int x) { stack.push_back(x); }
    int pop() {
        int x = stack.back();
        stack.pop_back();
        return x;
    }
    int top() { return stack.back(); }
    int peekMax() { return *max_element(stack.begin(), stack.end()); }
    int popMax() {
        int maxValue = peekMax();
        vector<int> buffer;
        while (top() != maxValue) buffer.push_back(pop());
        pop();
        for (int i = (int)buffer.size() - 1; i >= 0; i--) push(buffer[i]);
        return maxValue;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int t;
    if (!(cin >> t)) return 0;
    vector<string> cases;
    while (t--) {
        int ops;
        cin >> ops;
        MaxStack ms;
        vector<string> out;
        for (int i = 0; i < ops; i++) {
            string op;
            cin >> op;
            if (op == "C") {
                ms = MaxStack();
                out.push_back("null");
            } else if (op == "P") {
                int x;
                cin >> x;
                ms.push(x);
                out.push_back("null");
            } else if (op == "O") {
                out.push_back(to_string(ms.pop()));
            } else if (op == "T") {
                out.push_back(to_string(ms.top()));
            } else if (op == "M") {
                out.push_back(to_string(ms.peekMax()));
            } else {
                out.push_back(to_string(ms.popMax()));
            }
        }
        string line = "[";
        for (int i = 0; i < (int)out.size(); i++) {
            if (i) line += ",";
            line += out[i];
        }
        line += "]";
        cases.push_back(line);
    }
    for (int i = 0; i < (int)cases.size(); i++) {
        if (i) cout << "\n\n";
        cout << cases[i];
    }
}
