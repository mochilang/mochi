// Generated by Mochi 0.10.32 on 2025-07-26 20:33 +0700
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>


static char* str_int(int v) {
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

int main(void) {
    {
        long long __start = _now();
        long long __mem_start = _mem();
        {
            int i_arr[] = {1, 2, 3, 4, 5};
            size_t i_len = sizeof(i_arr) / sizeof(i_arr[0]);
            for (size_t __i = 0; __i < i_len; __i++) {
                int i = i_arr[__i];
                puts(str_int(i * i));
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
