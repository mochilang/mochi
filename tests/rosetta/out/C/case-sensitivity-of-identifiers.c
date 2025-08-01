// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:06Z
// Generated by Mochi compiler v0.10.28 on 2025-07-18T10:18:06Z
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *concat_string(char *a, char *b) {
  size_t len1 = strlen(a);
  size_t len2 = strlen(b);
  char *buf = (char *)malloc(len1 + len2 + 1);
  memcpy(buf, a, len1);
  memcpy(buf + len1, b, len2);
  buf[len1 + len2] = '\0';
  return buf;
}
static char *_str(int v) {
  char *buf = (char *)malloc(32);
  sprintf(buf, "%d", v);
  return buf;
}
typedef struct {
  int pkg_dog;
  int dog;
  int pkg__d_o_g;
} tmp_item_t;
typedef struct {
  int len;
  tmp_item_t *data;
} tmp_item_list_t;
tmp_item_list_t create_tmp_item_list(int len) {
  tmp_item_list_t l;
  l.len = len;
  l.data = calloc(len, sizeof(tmp_item_t));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}

int packageSees(char *d1, char *d2, char *d3) {
  char *tmp1 = concat_string("Package sees: ", d1);
  char *tmp2 = concat_string(tmp1, " ");
  char *tmp3 = concat_string(tmp2, d2);
  char *tmp4 = concat_string(tmp3, " ");
  char *tmp5 = concat_string(tmp4, d3);
  printf("%s\n", tmp5);
  return (tmp_item_t){.pkg_dog = 1, .dog = 1, .pkg__d_o_g = 1};
}

int mochi_main() {
  char *pkg_dog = "Salt";
  char *Dog = "Pepper";
  char *pkg_DOG = "Mustard";
  int d = packageSees(pkg_dog, Dog, pkg_DOG);
  char *tmp6 = _str(d.len);
  char *tmp7 = concat_string("There are ", tmp6);
  char *tmp8 = concat_string(tmp7, " dogs.\n");
  printf("%s\n", tmp8);
  char *dog = "Benjamin";
  d = packageSees(pkg_dog, Dog, pkg_DOG);
  char *tmp9 = concat_string("Main sees:   ", dog);
  char *tmp10 = concat_string(tmp9, " ");
  char *tmp11 = concat_string(tmp10, Dog);
  char *tmp12 = concat_string(tmp11, " ");
  char *tmp13 = concat_string(tmp12, pkg_DOG);
  printf("%s\n", tmp13);
  d.data["dog"] = 1;
  d.data["Dog"] = 1;
  d.data["pkg_DOG"] = 1;
  char *tmp14 = _str(d.len);
  char *tmp15 = concat_string("There are ", tmp14);
  char *tmp16 = concat_string(tmp15, " dogs.\n");
  printf("%s\n", tmp16);
  Dog = "Samba";
  d = packageSees(pkg_dog, Dog, pkg_DOG);
  char *tmp17 = concat_string("Main sees:   ", dog);
  char *tmp18 = concat_string(tmp17, " ");
  char *tmp19 = concat_string(tmp18, Dog);
  char *tmp20 = concat_string(tmp19, " ");
  char *tmp21 = concat_string(tmp20, pkg_DOG);
  printf("%s\n", tmp21);
  d.data["dog"] = 1;
  d.data["Dog"] = 1;
  d.data["pkg_DOG"] = 1;
  char *tmp22 = _str(d.len);
  char *tmp23 = concat_string("There are ", tmp22);
  char *tmp24 = concat_string(tmp23, " dogs.\n");
  printf("%s\n", tmp24);
  char *DOG = "Bernie";
  d = packageSees(pkg_dog, Dog, pkg_DOG);
  char *tmp25 = concat_string("Main sees:   ", dog);
  char *tmp26 = concat_string(tmp25, " ");
  char *tmp27 = concat_string(tmp26, Dog);
  char *tmp28 = concat_string(tmp27, " ");
  char *tmp29 = concat_string(tmp28, DOG);
  printf("%s\n", tmp29);
  d.data["dog"] = 1;
  d.data["Dog"] = 1;
  d.data["pkg_DOG"] = 1;
  d.data["DOG"] = 1;
  char *tmp30 = _str(d.len);
  char *tmp31 = concat_string("There are ", tmp30);
  char *tmp32 = concat_string(tmp31, " dogs.");
  printf("%s\n", tmp32);
}

int _mochi_main() {
  mochi_main();
  return 0;
}
int main() { return _mochi_main(); }
