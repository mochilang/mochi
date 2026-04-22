#include <algorithm>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>
using namespace std;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int d, e;
        cin >> d >> e;
        map<int, string> deptName;
        for (int i = 0; i < d; ++i) {
            int id; string name;
            cin >> id >> name;
            deptName[id] = name;
        }
        map<int, vector<pair<string, int>>> groups;
        for (int i = 0; i < e; ++i) {
            int id, salary, deptId;
            string name;
            cin >> id >> name >> salary >> deptId;
            groups[deptId].push_back({name, salary});
        }
        vector<tuple<string, int, string>> out;
        for (auto &entry : groups) {
            vector<int> salaries;
            set<int> seen;
            for (auto &p : entry.second) if (seen.insert(p.second).second) salaries.push_back(p.second);
            sort(salaries.begin(), salaries.end(), greater<int>());
            set<int> keep;
            for (int i = 0; i < (int)salaries.size() && i < 3; ++i) keep.insert(salaries[i]);
            for (auto &p : entry.second) if (keep.count(p.second)) out.push_back({deptName[entry.first], -p.second, p.first});
        }
        sort(out.begin(), out.end());
        cout << out.size() << '\n';
        for (int i = 0; i < (int)out.size(); ++i) {
            auto [dept, negSalary, name] = out[i];
            cout << dept << "," << name << "," << -negSalary;
            if (i + 1 < (int)out.size()) cout << '\n';
        }
        if (tc + 1 < t) cout << "\n\n";
    }
    return 0;
}
