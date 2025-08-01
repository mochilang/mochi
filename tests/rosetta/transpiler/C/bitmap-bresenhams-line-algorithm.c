// Generated by Mochi 0.10.32 on 2025-08-02 20:43 +0700
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

size_t bresenham_len;

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

typedef struct Point Point;

struct Point {
    int x;
    int y;
};

static Point* list_append_Point(Point *arr, size_t *len, Point val) {
    arr = realloc(arr, (*len + 1) * sizeof(Point));
    arr[*len] = val;
    (*len)++;
    return arr;
}

int absi(int x);
Point * bresenham(int x0, int y0, int x1, int y1);
void user_main();
int main(void);

int absi(int x) {
    if (x < 0LL) {
        return -(x);
    }
    return x;
}

Point * bresenham(int x0, int y0, int x1, int y1) {
    int dx = absi(x1 - x0);
    int dy = absi(y1 - y0);
    int sx = -1LL;
    if (x0 < x1) {
        sx = 1LL;
    }
    int sy = -1LL;
    if (y0 < y1) {
        sy = 1LL;
    }
    int err = dx - dy;
    Point *pts = NULL;
    size_t pts_len = 0;
    while (1LL) {
        pts = list_append_Point(pts, &pts_len, (Point){.x = x0, .y = y0});
        if ((x0 == x1) && (y0 == y1)) {
            break;
        }
        int e2 = 2LL * err;
        if (e2 > -(dy)) {
            err = err - dy;
            x0 = x0 + sx;
        }
        if (e2 < dx) {
            err = err + dx;
            y0 = y0 + sy;
        }
    }
    return bresenham_len = pts_len, pts;
}

void user_main() {
    Point *pts = bresenham(0LL, 0LL, 6LL, 4LL);
    size_t pts_len = bresenham_len;
    int i = 0LL;
    while (i < pts_len) {
        Point p = pts[(int)(i)];
        puts(str_concat(str_concat(str_concat(str_concat("(", str_int(p.x)), ","), str_int(p.y)), ")"));
        i = i + 1LL;
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
