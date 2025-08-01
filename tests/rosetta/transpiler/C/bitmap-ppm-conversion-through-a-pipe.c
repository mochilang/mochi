// Generated by Mochi 0.10.32 on 2025-07-31 12:13 +0700
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>


static int* list_append_int_new(const int *arr, size_t len, int val) {
    int *res = malloc((len + 1) * sizeof(int));
    if (arr && len) memcpy(res, arr, len * sizeof(int));
    res[len] = val;
    return res;
}

static int** list_append_intptr(int **arr, size_t *len, int *val) {
    arr = realloc(arr, (*len + 1) * sizeof(int*));
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

typedef struct Bitmap Bitmap;
typedef struct Pixel Pixel;

struct Bitmap {
    int cols;
    int rows;
    Pixel[] *px;
    size_t px_len;
};

struct Pixel {
    int R;
    int G;
    int B;
};

Pixel pixelFromRgb(int c);
int rgbFromPixel(Pixel p);
Bitmap NewBitmap(int x, int y);
int FillRgb(Bitmap b, int c);
int SetPxRgb(Bitmap b, int x, int y, int c);
int nextRand(int seed);
int user_main();
int main(void);

Pixel pixelFromRgb(int c) {
    int r = (c / 65536LL) % 256LL;
    int g = (c / 256LL) % 256LL;
    int b = c % 256LL;
    return (Pixel){.R = r, .G = g, .B = b};
}

int rgbFromPixel(Pixel p) {
    return ((p.R * 65536LL) + (p.G * 256LL)) + p.B;
}

Bitmap NewBitmap(int x, int y) {
    Pixel **data = NULL;
    size_t data_len = 0;
    size_t *data_lens = NULL;
    size_t data_lens_len = 0;
    int row = 0LL;
    while (row < y) {
        Pixel *r = NULL;
        size_t r_len = 0;
        int col = 0LL;
        while (col < x) {
                        r = list_append_int_new(r, r_len, (Pixel){.R = 0LL, .G = 0LL, .B = 0LL});
            col = col + 1LL;
        }
        data = list_append_strptr(data, &data_len, r);
        data_lens = list_append_szt(data_lens, &data_lens_len, r_len);
        row = row + 1LL;
    }
    return (Bitmap){.cols = x, .rows = y, .px = data, .px_len = data_len};
}

int FillRgb(Bitmap b, int c) {
    int y = 0LL;
    Pixel p = pixelFromRgb(c);
    while (y < b.rows) {
        int x = 0LL;
        while (x < b.cols) {
            Pixel **px = b.px;
            size_t px_len = b.px_len;
            size_t *px_lens = b.px_lens;
            size_t px_lens_len = b.px_len;
            Pixel *row = px[(int)(y)];
            size_t row_len = px_lens[y];
            row[(int)(x)] = p;
            px[(int)(y)] = row;
            b.px = px;
            x = x + 1LL;
        }
        y = y + 1LL;
    }
}

int SetPxRgb(Bitmap b, int x, int y, int c) {
    if ((((x < 0LL) || (x >= b.cols)) || (y < 0LL)) || (y >= b.rows)) {
        return 0LL;
    }
    Pixel **px = b.px;
    size_t px_len = b.px_len;
    size_t *px_lens = b.px_lens;
    size_t px_lens_len = b.px_len;
    Pixel *row = px[(int)(y)];
    size_t row_len = px_lens[y];
    row[(int)(x)] = pixelFromRgb(c);
    px[(int)(y)] = row;
    b.px = px;
    return 1LL;
}

int nextRand(int seed) {
    return ((seed * 1664525LL) + 1013904223LL) % 2147483648LL;
}

int user_main() {
    Bitmap bm = NewBitmap(400LL, 300LL);
    FillRgb(bm, 12615744LL);
    int seed = _now();
    int i = 0LL;
    while (i < 2000LL) {
        seed = nextRand(seed);
        int x = seed % 400LL;
        seed = nextRand(seed);
        int y = seed % 300LL;
        SetPxRgb(bm, x, y, 8405024LL);
        i = i + 1LL;
    }
    int x = 0LL;
    while (x < 400LL) {
        int y = 240LL;
        while (y < 245LL) {
            SetPxRgb(bm, x, y, 8405024LL);
            y = y + 1LL;
        }
        y = 260LL;
        while (y < 265LL) {
            SetPxRgb(bm, x, y, 8405024LL);
            y = y + 1LL;
        }
        x = x + 1LL;
    }
    int y = 0LL;
    while (y < 300LL) {
        int x = 80LL;
        while (x < 85LL) {
            SetPxRgb(bm, x, y, 8405024LL);
            x = x + 1LL;
        }
        x = 95LL;
        while (x < 100LL) {
            SetPxRgb(bm, x, y, 8405024LL);
            x = x + 1LL;
        }
        y = y + 1LL;
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
