// Generated by Mochi 0.10.32 on 2025-07-27 21:12 +0700
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>
#include <math.h>

size_t randInt_len;

static char* str_concat(const char *a, const char *b) {
    size_t len1 = strlen(a);
    size_t len2 = strlen(b);
    char *res = malloc(len1 + len2 + 1);
    memcpy(res, a, len1);
    memcpy(res + len1, b, len2);
    res[len1 + len2] = 0;
    return res;
}

static const char** list_append_str(const char **arr, size_t *len, const char *val) {
    arr = realloc((void*)arr, (*len + 1) * sizeof(char*));
    arr[*len] = val;
    (*len)++;
    return arr;
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

double xMin = -(2.182);
double xMax = 2.6558;
int yMin = 0;
double yMax = 9.9983;
int width = 60;
int nIter = 10000;
const char* **grid = NULL;
size_t grid_len = 0;
size_t *grid_lens = NULL;
size_t grid_lens_len = 0;
int row = 0;
int seed = 1;
double x = 0;
double y = 0;
int i = 0;

int * randInt(int s, int n);
int main(void);

int * randInt(int s, int n) {
    int next = ((s * 1664525) + 1013904223) % 2147483647;
    return randInt_len = 2, (int[]){ next, next % n };
}

int main(void) {
    {
        long long __start = _now();
        long long __mem_start = _mem();
        double dx = xMax - xMin;
        double dy = yMax - yMin;
        int height = (width * dy) / dx;
        while (row < height) {
            const char* *line = NULL;
            size_t line_len = 0;
            int col = 0;
            while (col < width) {
                line = list_append_str(line, &line_len, " ");
                col = col + 1;
            }
            grid = list_append_strptr(grid, &grid_len, line);
            grid_lens = list_append_szt(grid_lens, &grid_lens_len, line_len);
            row = row + 1;
        }
        double ix = ((double)(width) * (x - xMin)) / dx;
        double iy = ((double)(height) * (yMax - y)) / dy;
        if ((((ix >= 0) && (ix < width)) && (iy >= 0)) && (iy < height)) {
            grid[(int)(iy)][(int)(ix)] = "*";
        }
        while (i < nIter) {
            int *res = randInt(seed, 100);
            size_t res_len = randInt_len;
            seed = res[(int)(0)];
            int r = res[(int)(1)];
            if (r < 85) {
                double nx = (0.85 * x) + (0.04 * y);
                double ny = ((-(0.04) * x) + (0.85 * y)) + 1.6;
                x = nx;
                y = ny;
            } else {
                if (r < 92) {
                    double nx = (0.2 * x) - (0.26 * y);
                    double ny = ((0.23 * x) + (0.22 * y)) + 1.6;
                    x = nx;
                    y = ny;
                } else {
                    if (r < 99) {
                        double nx = (-(0.15) * x) + (0.28 * y);
                        double ny = ((0.26 * x) + (0.24 * y)) + 0.44;
                        x = nx;
                        y = ny;
                    } else {
                        x = 0;
                        y = 0.16 * y;
                    }
                }
            }
            ix = ((double)(width) * (x - xMin)) / dx;
            iy = ((double)(height) * (yMax - y)) / dy;
            if ((((ix >= 0) && (ix < width)) && (iy >= 0)) && (iy < height)) {
                grid[(int)(iy)][(int)(ix)] = "*";
            }
            i = i + 1;
        }
        row = 0;
        while (row < height) {
            const char* line = "";
            int col = 0;
            while (col < width) {
                line = str_concat(line, grid[(int)(row)][(int)(col)]);
                col = col + 1;
            }
            puts(line);
            row = row + 1;
        }
        long long __end = _now();
        long long __mem_end = _mem();
        long long __dur_us = (__end - __start) / 1000;
        long long __mem_bytes = __mem_end - __mem_start;
        printf("{\n  \"duration_us\": %-lld,\n  \"memory_bytes\": %-lld,\n  \"name\": \"main\"\n}\n", __dur_us, __mem_bytes);
    }
    return 0;
}
