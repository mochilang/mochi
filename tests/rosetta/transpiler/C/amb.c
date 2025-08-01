// Generated by Mochi 0.10.32 on 2025-07-25 21:49 +0700
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>


static char* str_concat(const char *a, const char *b) {
    size_t len1 = strlen(a);
    size_t len2 = strlen(b);
    char *res = malloc(len1 + len2 + 1);
    memcpy(res, a, len1);
    memcpy(res + len1, b, len2);
    res[len1 + len2] = 0;
    return res;
}

static int* list_append_int(int *arr, size_t *len, int val) {
    arr = realloc(arr, (*len + 1) * sizeof(int));
    arr[*len] = val;
    (*len)++;
    return arr;
}

static const char** list_append_str(const char **arr, size_t *len, const char *val) {
    arr = realloc((void*)arr, (*len + 1) * sizeof(char*));
    arr[*len] = val;
    (*len)++;
    return arr;
}

static const char*** list_append_strptr(const char ***arr, size_t *len, const char **val) {
    arr = realloc(arr, (*len + 1) * sizeof(const char**));
    arr[*len] = val;
    (*len)++;
    return arr;
}

static size_t* list_append_szt(size_t *arr, size_t *len, size_t val) {
    arr = realloc(arr, (*len + 1) * sizeof(size_t));
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

static char* _substring(const char *s, int start, int end) {
    int len = (int)strlen(s);
    if (start < 0) start = 0;
    if (end > len) end = len;
    if (start > end) start = end;
    char *res = malloc(end - start + 1);
    memcpy(res, s + start, end - start);
    res[end - start] = 0;
    return res;
}

int amb(const char* * * wordsets, size_t wordsets_len, size_t* wordsets_lens, size_t wordsets_lens_len, const char* * res, size_t res_len, int idx) {
    if (idx == wordsets_len) {
        return 1;
    }
    const char* prev = "";
    if (idx > 0) {
        prev = res[idx - 1];
    }
    int i = 0;
    while (i < wordsets_lens[idx]) {
        const char* w = wordsets[idx][i];
        if ((idx == 0) || (strcmp(_substring(prev, strlen(prev) - 1, strlen(prev)), _substring(w, 0, 1)) == 0)) {
            res[idx] = w;
            if (amb(wordsets, wordsets_len, wordsets_lens, wordsets_len, res, res_len, idx + 1)) {
                return 1;
            }
        }
        i = i + 1;
    }
    return 0;
}

int user_main() {
    const char* **wordset = NULL;
    size_t wordset_len = 0;
    size_t *wordset_lens = NULL;
    size_t wordset_lens_len = 0;
    wordset = list_append_strptr(wordset, &wordset_len, (const char*[]){"the", "that", "a"});
    wordset_lens = list_append_szt(wordset_lens, &wordset_lens_len, 3);
    wordset = list_append_strptr(wordset, &wordset_len, (const char*[]){"frog", "elephant", "thing"});
    wordset_lens = list_append_szt(wordset_lens, &wordset_lens_len, 3);
    wordset = list_append_strptr(wordset, &wordset_len, (const char*[]){"walked", "treaded", "grows"});
    wordset_lens = list_append_szt(wordset_lens, &wordset_lens_len, 3);
    wordset = list_append_strptr(wordset, &wordset_len, (const char*[]){"slowly", "quickly"});
    wordset_lens = list_append_szt(wordset_lens, &wordset_lens_len, 2);
    const char* *res = NULL;
    size_t res_len = 0;
    int i = 0;
    while (i < 4) {
        res = list_append_str(res, &res_len, "");
        i = i + 1;
    }
    if (amb(wordset, wordset_len, wordset_lens, wordset_len, res, res_len, 0)) {
        const char* out = str_concat("[", res[0]);
        int j = 1;
        while (j < res_len) {
            out = str_concat(str_concat(out, " "), res[j]);
            j = j + 1;
        }
        out = str_concat(out, "]");
        puts(out);
    } else {
        puts("No amb found");
    }
}

int main(void) {
    {
        long long __start = _now();
        long long __mem_start = _mem();
        user_main();
        long long __end = _now();
        long long __mem_end = _mem();
        long long __dur_us = (__end - __start) / 1000;
        long long __mem_bytes = __mem_end - __mem_start;
        printf("{\n  \"duration_us\": %-lld,\n  \"memory_bytes\": %-lld,\n  \"name\": \"main\"\n}\n", __dur_us, __mem_bytes);
    }
    return 0;
}
