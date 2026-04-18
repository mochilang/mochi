#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void get_permutation(int n, int k, char *out) {
  char digits[10];
  int fact[10];
  for (int i = 0; i < n; i++) digits[i] = '1' + i;
  fact[0] = 1;
  for (int i = 1; i <= n; i++) fact[i] = fact[i - 1] * i;
  k--;
  int len = n;
  for (int pos = 0; pos < n; pos++) {
    int rem = n - pos;
    int block = fact[rem - 1];
    int idx = k / block;
    k %= block;
    out[pos] = digits[idx];
    memmove(&digits[idx], &digits[idx + 1], len - idx - 1);
    len--;
  }
  out[n] = '\0';
}

int main() {
  int t;
  if (scanf("%d", &t) != 1) return 0;
  for (int tc = 0; tc < t; tc++) {
    int n, k;
    scanf("%d", &n);
    scanf("%d", &k);
    char out[16];
    get_permutation(n, k, out);
    printf("%s", out);
    if (tc + 1 < t) printf("\n");
  }
  return 0;
}
