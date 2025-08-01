// Generated by Mochi 0.10.32 on 2025-08-02 14:23 +0700
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

size_t split_len;

static char* str_concat(const char *a, const char *b) {
    size_t len1 = strlen(a);
    size_t len2 = strlen(b);
    char *res = malloc(len1 + len2 + 1);
    memcpy(res, a, len1);
    memcpy(res + len1, b, len2);
    res[len1 + len2] = 0;
    return res;
}

static char* str_int(int v) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", v);
    return strdup(buf);
}

typedef struct { const char **keys; int *vals; size_t len; } MapSI;

static int map_get_si(const char *keys[], const int vals[], size_t len, const char *key) {
    for (size_t i = 0; i < len; i++) {
        if (strcmp(keys[i], key) == 0) return vals[i];
    }
    return 0;
}

static const char** list_append_str(const char **arr, size_t *len, const char *val) {
    arr = realloc((void*)arr, (*len + 1) * sizeof(char*));
    arr[*len] = val;
    (*len)++;
    return arr;
}

#include <time.h>
#include <stdlib.h>
static int seeded_now = 0;
static long long now_seed = 0;
static long long _now(void) {
    if (!seeded_now) {
        const char *s = getenv("MOCHI_NOW_SEED");
        if (s && *s) {
            now_seed = atoll(s);
            seeded_now = 1;
        }
    }
    if (seeded_now) {
        now_seed = (now_seed * 1664525 + 1013904223) % 2147483647;
        return now_seed;
    }
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (long long)(ts.tv_sec * 1000000000LL + ts.tv_nsec);
}

static long long _mem(void) {
    struct mallinfo mi = mallinfo();
    return (long long)mi.uordblks;
}

static const char* _substring(const char *s, int start, int end) {
    int len = (int)strlen(s);
    if (start < 0) start = 0;
    if (end > len) end = len;
    if (start > end) start = end;
    char *res = malloc(end - start + 1);
    memcpy(res, s + start, end - start);
    res[end - start] = 0;
    return res;
}

const char* tests_init[6] = {"87.70.141.1/22", "36.18.154.103/12", "62.62.197.11/29", "67.137.119.181/4", "161.214.74.21/24", "184.232.176.184/18"};
const char* *tests = tests_init;
size_t tests_len = 6;

const char* * split(const char* s, const char* sep);
const char* join(const char* * xs, size_t xs_len, const char* sep);
const char* repeat(const char* ch, int n);
int parseIntStr(const char* str);
const char* toBinary(int n, int bits);
int binToInt(const char* bits);
const char* padRight(const char* s, int width);
const char* canonicalize(const char* cidr);
int main(void);

const char* * split(const char* s, const char* sep) {
    const char* *parts = NULL;
    size_t parts_len = 0;
    const char* cur = "";
    int i = 0LL;
    while (i < strlen(s)) {
        if (((strlen(sep) > 0LL) && ((i + strlen(sep)) <= strlen(s))) && (strcmp(_substring(s, i, i + strlen(sep)), sep) == 0)) {
            parts = list_append_str(parts, &parts_len, cur);
            cur = "";
            i = i + strlen(sep);
        } else {
            cur = str_concat(cur, _substring(s, i, i + 1LL));
            i = i + 1LL;
        }
    }
    parts = list_append_str(parts, &parts_len, cur);
    return split_len = parts_len, parts;
}

const char* join(const char* * xs, size_t xs_len, const char* sep) {
    const char* res = "";
    int i = 0LL;
    while (i < xs_len) {
        if (i > 0LL) {
            res = str_concat(res, sep);
        }
        res = str_concat(res, xs[(int)(i)]);
        i = i + 1LL;
    }
    return res;
}

const char* repeat(const char* ch, int n) {
    const char* out = "";
    int i = 0LL;
    while (i < n) {
        out = str_concat(out, ch);
        i = i + 1LL;
    }
    return out;
}

int parseIntStr(const char* str) {
    int i = 0LL;
    int neg = 0LL;
    if ((strlen(str) > 0LL) && (strcmp(_substring(str, 0LL, 1LL), "-") == 0)) {
        neg = 1LL;
        i = 1LL;
    }
    int n = 0LL;
    const char* digits_keys[26] = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"};
    int digits_vals[26] = {0LL, 1LL, 2LL, 3LL, 4LL, 5LL, 6LL, 7LL, 8LL, 9LL};
    size_t digits_len = 10;
    MapSI digits = { digits_keys, digits_vals, digits_len };
    while (i < strlen(str)) {
        n = (n * 10LL) + map_get_si(digits_keys, digits_vals, digits_len, _substring(str, i, i + 1LL));
        i = i + 1LL;
    }
    if (neg) {
        n = -(n);
    }
    return n;
}

const char* toBinary(int n, int bits) {
    const char* b = "";
    int val = n;
    int i = 0LL;
    while (i < bits) {
        b = str_concat(str_int(val % 2LL), b);
        val = val / 2LL;
        i = i + 1LL;
    }
    return b;
}

int binToInt(const char* bits) {
    int n = 0LL;
    int i = 0LL;
    while (i < strlen(bits)) {
        n = (n * 2LL) + parseIntStr(_substring(bits, i, i + 1LL));
        i = i + 1LL;
    }
    return n;
}

const char* padRight(const char* s, int width) {
    const char* out = s;
    while (strlen(out) < width) {
        out = str_concat(out, " ");
    }
    return out;
}

const char* canonicalize(const char* cidr) {
    const char* *parts = split(cidr, "/");
    size_t parts_len = split_len;
    const char* dotted = parts[(int)(0LL)];
    int size = parseIntStr(parts[(int)(1LL)]);
    const char* *binParts = NULL;
    size_t binParts_len = 0;
    {
        const char** __tmp0 = split(dotted, ".");
        size_t __tmp0_len = split_len;
        for (size_t __i = 0; __i < __tmp0_len; __i++) {
            const char* p = __tmp0[__i];
            binParts = list_append_str(binParts, &binParts_len, toBinary(parseIntStr(p), 8LL));
        }
    }
    const char* binary = join(binParts, binParts_len, "");
    binary = str_concat(_substring(binary, 0LL, size), repeat("0", 32LL - size));
    const char* *canonParts = NULL;
    size_t canonParts_len = 0;
    int i = 0LL;
    while (i < strlen(binary)) {
        canonParts = list_append_str(canonParts, &canonParts_len, str_int(binToInt(_substring(binary, i, i + 8LL))));
        i = i + 8LL;
    }
    return str_concat(str_concat(join(canonParts, canonParts_len, "."), "/"), parts[(int)(1LL)]);
}

int main(void) {
    {
        long long __start = _now();
        long long __mem_start = _mem();
        {
            const char* t_arr[] = {"87.70.141.1/22", "36.18.154.103/12", "62.62.197.11/29", "67.137.119.181/4", "161.214.74.21/24", "184.232.176.184/18"};
            size_t t_len = sizeof(t_arr) / sizeof(t_arr[0]);
            for (size_t __i = 0; __i < t_len; __i++) {
                const char* t = t_arr[__i];
                puts(str_concat(str_concat(padRight(t, 18LL), " -> "), canonicalize(t)));
            }
        }
        long long __end = _now();
        long long __mem_end = _mem();
        long long __dur_us = (__end - __start) / 1000;
        long long __mem_bytes = __mem_end - __mem_start;
        printf("{\n  \"duration_us\": %-lld,\n  \"memory_bytes\": %-lld,\n  \"name\": \"main\"\n}\n", __dur_us, __mem_bytes);
    }
    return 0;
}
