// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:11:47Z
// Generated by Mochi compiler v0.10.30 on 2025-07-18T17:11:47Z
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  int len;
  int *data;
} list_int;
static list_int list_int_create(int len) {
  list_int l;
  l.len = len;
  l.data = calloc(len, sizeof(int));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
typedef struct {
  int len;
  char **data;
} list_string;
static list_string list_string_create(int len) {
  list_string l;
  l.len = len;
  l.data = calloc(len, sizeof(char *));
  if (!l.data && len > 0) {
    fprintf(stderr, "alloc failed\n");
    exit(1);
  }
  return l;
}
static list_int concat_list_int(list_int a, list_int b) {
  list_int r = list_int_create(a.len + b.len);
  for (int i = 0; i < a.len; i++)
    r.data[i] = a.data[i];
  for (int i = 0; i < b.len; i++)
    r.data[a.len + i] = b.data[i];
  return r;
}
static list_string concat_list_string(list_string a, list_string b) {
  list_string r = list_string_create(a.len + b.len);
  for (int i = 0; i < a.len; i++)
    r.data[i] = a.data[i];
  for (int i = 0; i < b.len; i++)
    r.data[a.len + i] = b.data[i];
  return r;
}
static char *concat_string(char *a, char *b) {
  size_t len1 = strlen(a);
  size_t len2 = strlen(b);
  char *buf = (char *)malloc(len1 + len2 + 1);
  memcpy(buf, a, len1);
  memcpy(buf + len1, b, len2);
  buf[len1 + len2] = '\0';
  return buf;
}
static char *slice_string(char *s, int start, int end) {
  int len = strlen(s);
  if (start < 0)
    start += len;
  if (end < 0)
    end += len;
  if (start < 0)
    start = 0;
  if (end > len)
    end = len;
  if (start > end)
    start = end;
  char *buf = (char *)malloc(end - start + 1);
  memcpy(buf, s + start, end - start);
  buf[end - start] = '\0';
  return buf;
}
static list_string slice_list_string(list_string v, int start, int end) {
  if (start < 0)
    start += v.len;
  if (end < 0)
    end += v.len;
  if (start < 0)
    start = 0;
  if (end > v.len)
    end = v.len;
  if (start > end)
    start = end;
  list_string r = list_string_create(end - start);
  for (int i = 0; i < r.len; i++)
    r.data[i] = v.data[start + i];
  return r;
}
static char *text;
static int f;

typedef struct {
  int text;
  list_int width;
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

list_string split(char *s, char *sep) {
  list_string parts = list_string_create(0);
  __auto_type cur = "";
  __auto_type i = 0;
  while (i < strlen(s)) {
    if (strlen(sep) > 0 && i + strlen(sep) <= strlen(s) &&
        slice_string(s, i, i + strlen(sep)) == sep) {
      list_string tmp1 = list_string_create(1);
      tmp1.data[0] = cur;
      list_string tmp2 = concat_list_string(parts, tmp1);
      parts = tmp2;
      cur = "";
      i = i + strlen(sep);
    } else {
      char *tmp3 = concat_string(cur, slice_string(s, i, i + 1));
      cur = tmp3;
      i = i + 1;
    }
  }
  list_string tmp4 = list_string_create(1);
  tmp4.data[0] = cur;
  list_string tmp5 = concat_list_string(parts, tmp4);
  parts = tmp5;
  return parts;
}

list_string rstripEmpty(list_string words) {
  __auto_type n = words.len;
  while (n > 0 && words.data[n - 1] == "") {
    n = n - 1;
  }
  list_string tmp6 = slice_list_string(words, 0, n);
  return tmp6;
}

char *spaces(int n) {
  __auto_type out = "";
  __auto_type i = 0;
  while (i < n) {
    char *tmp7 = concat_string(out, " ");
    out = tmp7;
    i = i + 1;
  }
  return out;
}

char *pad(char *word, int width, int align) {
  __auto_type diff = width - strlen(word);
  if (align == 0) {
    char *tmp8 = concat_string(word, spaces(diff));
    return tmp8;
  }
  if (align == 2) {
    char *tmp9 = concat_string(spaces(diff), word);
    return tmp9;
  }
  __auto_type left = (int)((diff / 2));
  __auto_type right = diff - left;
  char *tmp10 = concat_string(spaces(left), word);
  char *tmp11 = concat_string(tmp10, spaces(right));
  return tmp11;
}

int newFormatter(char *text) {
  __auto_type lines = split(text, "\n");
  list_int width = list_int_create(0);
  __auto_type i = 0;
  while (i < lines.len) {
    if (strlen(lines.data[i]) == 0) {
      i = i + 1;
      continue;
    }
    __auto_type words = rstripEmpty(split(lines.data[i], "$"));
    fmtLines = 0;
    __auto_type j = 0;
    while (j < words.len) {
      __auto_type wlen = strlen(words.data[j]);
      if ((j == 0)) {
        int tmp12_data[1];
        list_int tmp12 = {1, tmp12_data};
        tmp12.data[0] = wlen;
        list_int tmp13 = concat_list_int(width, tmp12);
        width = tmp13;
      } else if (wlen > width.data[j]) {
        width.data[j] = wlen;
      }
      j = j + 1;
    }
    i = i + 1;
  }
  return (tmp_item_t){.text = fmtLines, .width = width};
}

int printFmt(map_string_int f, int align) {
  __auto_type lines = (int)(f.data["text"]);
  __auto_type width = (list_int)(f.data["width"]);
  __auto_type i = 0;
  while (i < lines.len) {
    __auto_type words = lines.data[i];
    __auto_type line = "";
    __auto_type j = 0;
    while (j < words.len) {
      char *tmp14 =
          concat_string(line, pad(words.data[j], width.data[j], align));
      char *tmp15 = concat_string(tmp14, " ");
      line = tmp15;
      j = j + 1;
    }
    printf("%s\n", line);
    i = i + 1;
  }
  printf("\n");
}

int _mochi_main() {
  char *tmp16 = concat_string(
      "Given$a$text$file$of$many$lines,$where$fields$within$a$line\n",
      "are$delineated$by$a$single$'dollar'$character,$write$a$program\n");
  char *tmp17 = concat_string(
      tmp16,
      "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n");
  char *tmp18 =
      concat_string(tmp17, "column$are$separated$by$at$least$one$space.\n");
  char *tmp19 = concat_string(
      tmp18, "Further,$allow$for$each$word$in$a$column$to$be$either$left\n");
  char *tmp20 = concat_string(
      tmp19,
      "justified,$right$justified,$or$center$justified$within$its$column.");
  text = tmp20;
  f = newFormatter(text);
  printFmt(f, 0);
  printFmt(f, 1);
  printFmt(f, 2);
  return 0;
}
int main() { return _mochi_main(); }
