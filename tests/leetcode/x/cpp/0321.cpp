#include <iostream>
#include <string>
#include <vector>

using namespace std;

static vector<int> pick(const vector<int>& nums, int k) {
    int drop = nums.size() - k;
    vector<int> st;
    for (int x : nums) {
        while (drop > 0 && !st.empty() && st.back() < x) {
            st.pop_back();
            drop--;
        }
        st.push_back(x);
    }
    st.resize(k);
    return st;
}

static bool greaterSeq(const vector<int>& a, int i, const vector<int>& b, int j) {
    while (i < (int)a.size() && j < (int)b.size() && a[i] == b[j]) {
        i++;
        j++;
    }
    return j == (int)b.size() || (i < (int)a.size() && a[i] > b[j]);
}

static vector<int> mergeSeq(const vector<int>& a, const vector<int>& b) {
    vector<int> out;
    int i = 0, j = 0;
    while (i < (int)a.size() || j < (int)b.size()) {
        if (greaterSeq(a, i, b, j)) out.push_back(a[i++]);
        else out.push_back(b[j++]);
    }
    return out;
}

static vector<int> maxNumber(const vector<int>& nums1, const vector<int>& nums2, int k) {
    vector<int> best;
    int start = max(0, k - (int)nums2.size());
    int end = min(k, (int)nums1.size());
    for (int take = start; take <= end; ++take) {
        vector<int> cand = mergeSeq(pick(nums1, take), pick(nums2, k - take));
        if (greaterSeq(cand, 0, best, 0)) best = cand;
    }
    return best;
}

static string fmtList(const vector<int>& a) {
    string out = "[";
    for (int i = 0; i < (int)a.size(); ++i) {
        if (i) out += ",";
        out += to_string(a[i]);
    }
    return out + "]";
}

int main() {
    int t;
    if (!(cin >> t)) return 0;
    for (int tc = 0; tc < t; ++tc) {
        int n1, n2, k;
        cin >> n1;
        vector<int> nums1(n1);
        for (int& x : nums1) cin >> x;
        cin >> n2;
        vector<int> nums2(n2);
        for (int& x : nums2) cin >> x;
        cin >> k;
        if (tc) cout << "\n\n";
        cout << fmtList(maxNumber(nums1, nums2, k));
    }
    return 0;
}
