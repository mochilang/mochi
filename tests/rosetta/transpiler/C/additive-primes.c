// Generated by Mochi 0.10.32 on 2025-07-25 19:53 +0700
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

static char* str_int(int v) {
    if(v==0) return strdup("false");
    if(v==1) return strdup("true");
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", v);
    return strdup(buf);
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

int isPrime(int n) {
    if (n < 2) {
        return 0;
    }
    if ((n % 2) == 0) {
        return n == 2;
    }
    if ((n % 3) == 0) {
        return n == 3;
    }
    int d = 5;
    while ((d * d) <= n) {
        if ((n % d) == 0) {
            return 0;
        }
        d = d + 2;
        if ((n % d) == 0) {
            return 0;
        }
        d = d + 4;
    }
    return 1;
}

int sumDigits(int n) {
    int s = 0;
    int x = n;
    while (x > 0) {
        s = s + (x % 10);
        x = x / 10;
    }
    return s;
}

const char* pad(int n) {
    if (n < 10) {
        return str_concat("  ", str_int(n));
    }
    if (n < 100) {
        return str_concat(" ", str_int(n));
    }
    return str_int(n);
}

int user_main() {
    puts("Additive primes less than 500:");
    int count = 0;
    const char* line = "";
    int lineCount = 0;
    int i = 2;
    while (i < 500) {
        if (isPrime(i) && isPrime(sumDigits(i))) {
            count = count + 1;
            line = str_concat(str_concat(line, pad(i)), "  ");
            lineCount = lineCount + 1;
            if (lineCount == 10) {
                printf("%d\n", _substring(line, 0, strlen(line) - 2));
                line = "";
                lineCount = 0;
            }
        }
        if (i > 2) {
            i = i + 2;
        } else {
            i = i + 1;
        }
    }
    if (lineCount > 0) {
        printf("%d\n", _substring(line, 0, strlen(line) - 2));
    }
    puts(str_concat(str_int(count), " additive primes found."));
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
