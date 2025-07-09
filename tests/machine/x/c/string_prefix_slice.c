#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  char *prefix = "fore";
  char *s1 = "forest";
  char *_t1 = ({
    int _len = strlen(s1);
    int _s = 0;
    int _e = strlen(prefix);
    if (_s < 0)
      _s += _len;
    if (_e < 0)
      _e += _len;
    if (_s < 0)
      _s = 0;
    if (_e > _len)
      _e = _len;
    if (_s > _e)
      _s = _e;
    char *_b = (char *)malloc(_e - _s + 1);
    memcpy(_b, s1 + _s, _e - _s);
    _b[_e - _s] = '\0';
    _b;
  });
  printf("%s\n", ((strcmp(_t1, prefix) == 0)) ? "true" : "false");
  char *s2 = "desert";
  char *_t2 = ({
    int _len = strlen(s2);
    int _s = 0;
    int _e = strlen(prefix);
    if (_s < 0)
      _s += _len;
    if (_e < 0)
      _e += _len;
    if (_s < 0)
      _s = 0;
    if (_e > _len)
      _e = _len;
    if (_s > _e)
      _s = _e;
    char *_b = (char *)malloc(_e - _s + 1);
    memcpy(_b, s2 + _s, _e - _s);
    _b[_e - _s] = '\0';
    _b;
  });
  printf("%s\n", ((strcmp(_t2, prefix) == 0)) ? "true" : "false");
  return 0;
}
