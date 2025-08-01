// Generated by Mochi 0.10.32 on 2025-07-24 08:10 +0700
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

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

static char* str_float(double v) {
    char buf[64];
    snprintf(buf, sizeof(buf), "%g", v);
    return strdup(buf);
}

static int* list_append_int(int *arr, size_t *len, int val) {
    arr = realloc(arr, (*len + 1) * sizeof(int));
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
    return (long long)(ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
}

int * shuffle(int * xs, size_t xs_len) {
    int *arr = xs;
    int i = 99;
    while (i > 0) {
        int j = _now() % (i + 1);
        int tmp = arr[i];
        arr[i] = arr[j];
        arr[j] = tmp;
        i = i - 1;
    }
    return arr;
}

int doTrials(int trials, int np, const char* strategy) {
    int pardoned = 0;
    int t = 0;
    while (t < trials) {
        int *drawers = NULL;
        size_t drawers_len = 0;
        int i = 0;
        while (i < 100) {
            drawers = list_append_int(drawers, &drawers_len, i);
            i = i + 1;
        }
        drawers = shuffle(drawers, drawers_len);
        int p = 0;
        int success = 1;
        while (p < np) {
            int found = 0;
            if (strcmp(strategy, "optimal") == 0) {
                int prev = p;
                int d = 0;
                while (d < 50) {
                    int this = drawers[prev];
                    if (this == p) {
                        found = 1;
                        break;
                    }
                    prev = this;
                    d = d + 1;
                }
            } else {
                int *opened = NULL;
                size_t opened_len = 0;
                int k = 0;
                while (k < 100) {
                    opened = list_append_int(opened, &opened_len, 0);
                    k = k + 1;
                }
                int d = 0;
                while (d < 50) {
                    int n = _now() % 100;
                    while (opened[n]) {
                        n = _now() % 100;
                    }
                    opened[n] = 1;
                    if (drawers[n] == p) {
                        found = 1;
                        break;
                    }
                    d = d + 1;
                }
            }
            if (!(found)) {
                success = 0;
                break;
            }
            p = p + 1;
        }
        if (success) {
            pardoned = pardoned + 1;
        }
        t = t + 1;
    }
    double rf = ((double)(pardoned) / (double)(trials)) * 100;
    puts(str_concat(str_concat(str_concat(str_concat(str_concat(str_concat("  strategy = ", strategy), "  pardoned = "), str_int(pardoned)), " relative frequency = "), str_float(rf)), "%"));
}

int user_main() {
    int trials = 1000;
    {
        int np_arr[] = {10, 100};
        size_t np_len = sizeof(np_arr) / sizeof(np_arr[0]);
        for (size_t i = 0; i < np_len; i++) {
            int np = np_arr[i];
            puts(str_concat(str_concat(str_concat(str_concat("Results from ", str_int(trials)), " trials with "), str_int(np)), " prisoners:\n"));
            {
                const char* strat_arr[] = {"random", "optimal"};
                size_t strat_len = sizeof(strat_arr) / sizeof(strat_arr[0]);
                for (size_t i = 0; i < strat_len; i++) {
                    const char* strat = strat_arr[i];
                    doTrials(trials, np, strat);
                }
            }
        }
    }
}

int main(void) {
    user_main();
    return 0;
}
