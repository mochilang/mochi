#include <iostream>
#include <queue>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

struct TreeNode {
    int val;
    TreeNode* left;
    TreeNode* right;
    explicit TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
};

struct Codec {
    string serialize(TreeNode* root) {
        if (!root) return "[]";
        vector<string> out;
        queue<TreeNode*> q;
        q.push(root);
        while (!q.empty()) {
            TreeNode* node = q.front();
            q.pop();
            if (!node) {
                out.push_back("null");
            } else {
                out.push_back(to_string(node->val));
                q.push(node->left);
                q.push(node->right);
            }
        }
        while (!out.empty() && out.back() == "null") out.pop_back();
        string ans = "[";
        for (int i = 0; i < (int)out.size(); ++i) {
            if (i) ans += ",";
            ans += out[i];
        }
        ans += "]";
        return ans;
    }

    TreeNode* deserialize(string data) {
        if (data == "[]") return nullptr;
        vector<string> vals;
        string cur;
        for (int i = 1; i + 1 < (int)data.size(); ++i) {
            if (data[i] == ',') {
                vals.push_back(cur);
                cur.clear();
            } else {
                cur += data[i];
            }
        }
        vals.push_back(cur);
        TreeNode* root = new TreeNode(stoi(vals[0]));
        queue<TreeNode*> q;
        q.push(root);
        int i = 1;
        while (!q.empty() && i < (int)vals.size()) {
            TreeNode* node = q.front();
            q.pop();
            if (i < (int)vals.size() && vals[i] != "null") {
                node->left = new TreeNode(stoi(vals[i]));
                q.push(node->left);
            }
            ++i;
            if (i < (int)vals.size() && vals[i] != "null") {
                node->right = new TreeNode(stoi(vals[i]));
                q.push(node->right);
            }
            ++i;
        }
        return root;
    }
};

int main() {
    string line;
    if (!getline(cin, line)) return 0;
    int t = stoi(line);
    Codec codec;
    for (int tc = 0; tc < t; ++tc) {
        getline(cin, line);
        if (tc) cout << "\n\n";
        cout << codec.serialize(codec.deserialize(line));
    }
    return 0;
}
