#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  char *s = "mochi";
  char *_t1 = ({
    int _len = strlen(s);
    int _i = 1;
    if (_i < 0)
      _i += _len;
    if (_i < 0 || _i >= _len) {
      fprintf(stderr, "index out of range\n");
      exit(1);
    }
    char *_b = (char *)malloc(2);
    _b[0] = s[_i];
    _b[1] = '\0';
    _b;
  });
  printf("%s\n", _t1);
  return 0;
}
