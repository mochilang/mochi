#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
using namespace std;

struct Node {
    unordered_map<char, Node*> child;
    string word;
};

static vector<string> solve(vector<string> board, const vector<string>& words) {
    Node* root = new Node();
    for (const string& word : words) {
        Node* node = root;
        for (char ch : word) {
            if (!node->child.count(ch)) node->child[ch] = new Node();
            node = node->child[ch];
        }
        node->word = word;
    }
    int rows = (int)board.size(), cols = (int)board[0].size();
    vector<string> found;
    auto dfs = [&](auto&& self, int r, int c, Node* node) -> void {
        char ch = board[r][c];
        if (!node->child.count(ch)) return;
        Node* next = node->child[ch];
        if (!next->word.empty()) {
            found.push_back(next->word);
            next->word.clear();
        }
        board[r][c] = '#';
        if (r > 0 && board[r - 1][c] != '#') self(self, r - 1, c, next);
        if (r + 1 < rows && board[r + 1][c] != '#') self(self, r + 1, c, next);
        if (c > 0 && board[r][c - 1] != '#') self(self, r, c - 1, next);
        if (c + 1 < cols && board[r][c + 1] != '#') self(self, r, c + 1, next);
        board[r][c] = ch;
    };
    for (int r = 0; r < rows; ++r)
        for (int c = 0; c < cols; ++c)
            if (root->child.count(board[r][c])) dfs(dfs, r, c, root);
    sort(found.begin(), found.end());
    return found;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int rows, cols;
        cin >> rows >> cols;
        vector<string> board(rows);
        for (int i = 0; i < rows; ++i) cin >> board[i];
        int n;
        cin >> n;
        vector<string> words(n);
        for (int i = 0; i < n; ++i) cin >> words[i];
        vector<string> ans = solve(board, words);
        cout << ans.size();
        for (const string& s : ans) cout << '\n' << s;
        if (tc + 1 < t) cout << "\n\n";
    }
    return 0;
}
