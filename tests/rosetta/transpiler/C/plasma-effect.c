// Generated by Mochi 0.10.32 on 2025-08-01 23:10 +0700
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>
#include <math.h>


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

double PI = 3.141592653589793;
int nframes = 10LL;
int w = 32LL;
int h = 32LL;
int total = 0LL;
int f = 1LL;

double user_floorf(double x);
double frac(double x);
double sinApprox(double x);
double sqrtApprox(double x);
int main(void);

double user_floorf(double x) {
    int i = (int)(x);
    if ((double)(i) > x) {
        i = i - 1LL;
    }
    return (double)(i);
}

double frac(double x) {
    return x - user_floorf(x);
}

double sinApprox(double x) {
    double term = x;
    double sum = x;
    int n = 1LL;
    while (n <= 10LL) {
        double denom = (2LL * n) * ((2LL * n) + 1LL);
        term = ((-(term) * x) * x) / denom;
        sum = sum + term;
        n = n + 1LL;
    }
    return sum;
}

double sqrtApprox(double x) {
    if (x <= 0LL) {
        return 0;
    }
    double guess = x;
    int i = 0LL;
    while (i < 10LL) {
        guess = (guess + (x / guess)) / 2;
        i = i + 1LL;
    }
    return guess;
}

int main(void) {
    {
        long long __start = _now();
        long long __mem_start = _mem();
        while (f <= nframes) {
            int y = 0LL;
            while (y < h) {
                int x = 0LL;
                while (x < w) {
                    double fx = (double)(x);
                    double fy = (double)(y);
                    double value = sinApprox(fx / 16);
                    value = value + sinApprox(fy / 8);
                    value = value + sinApprox((fx + fy) / 16);
                    value = value + sinApprox(sqrtApprox((fx * fx) + (fy * fy)) / 8);
                    value = value + 4;
                    value = value / 8;
                    double rem = frac(value + ((double)(f) / (double)(nframes)));
                    int ci = ((double)(nframes) * rem) + 1LL;
                    total = total + ci;
                    x = x + 1LL;
                }
                y = y + 1LL;
            }
            f = f + 1LL;
        }
        printf("%d\n", total);
        long long __end = _now();
        long long __mem_end = _mem();
        long long __dur_us = (__end - __start) / 1000;
        long long __mem_bytes = __mem_end - __mem_start;
        printf("{\n  \"duration_us\": %-lld,\n  \"memory_bytes\": %-lld,\n  \"name\": \"main\"\n}\n", __dur_us, __mem_bytes);
    }
    return 0;
}
