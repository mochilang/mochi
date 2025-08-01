// Generated by Mochi 0.10.32 on 2025-07-26 18:02 +0000
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

static int _char_at(const char *s, int idx) {
    int len = (int)strlen(s);
    if (idx < 0 || idx >= len) return 0;
    return (unsigned char)s[idx];
}

typedef struct Parser Parser;
typedef struct Res Res;

struct Parser {
    const char* expr;
    int pos;
};

struct Res {
    int v;
    Parser p;
};

Parser skipWS(Parser p);
int parseIntStr(const char* str);
Res parseNumber(Parser p);
Res parseFactor(Parser p);
int powInt(int base, int exp);
Res parsePower(Parser p);
Res parseTerm(Parser p);
Res parseExpr(Parser p);
int evalExpr(const char* expr);
int user_main();
int main(void);

Parser skipWS(Parser p) {
    int i = p.pos;
    while ((i < strlen(p.expr)) && (strcmp(_substring(p.expr, i, i + 1), " ") == 0)) {
        i = i + 1;
    }
    p.pos = i;
    return p;
}

int parseIntStr(const char* str) {
    int i = 0;
    int n = 0;
    while (i < strlen(str)) {
        n = ((n * 10) + (int)(_char_at(_substring(str, i, i + 1), i))) - 48;
        i = i + 1;
    }
    return n;
}

Res parseNumber(Parser p) {
    p = skipWS(p);
    int start = p.pos;
    while (p.pos < strlen(p.expr)) {
        const char* ch = _substring(p.expr, p.pos, p.pos + 1);
        if ((strcmp(ch, "0") >= 0) && (strcmp(ch, "9") <= 0)) {
            p.pos = p.pos + 1;
        } else {
            break;
        }
    }
    const char* token = _substring(p.expr, start, p.pos);
    return (Res){.v = parseIntStr(token), .p = p};
}

Res parseFactor(Parser p) {
    p = skipWS(p);
    if ((p.pos < strlen(p.expr)) && (strcmp(_substring(p.expr, p.pos, p.pos + 1), "(") == 0)) {
        p.pos = p.pos + 1;
        Res r = parseExpr(p);
        int v = r.v;
        p = r.p;
        p = skipWS(p);
        if ((p.pos < strlen(p.expr)) && (strcmp(_substring(p.expr, p.pos, p.pos + 1), ")") == 0)) {
            p.pos = p.pos + 1;
        }
        return (Res){.v = v, .p = p};
    }
    if ((p.pos < strlen(p.expr)) && (strcmp(_substring(p.expr, p.pos, p.pos + 1), "-") == 0)) {
        p.pos = p.pos + 1;
        Res r = parseFactor(p);
        int v = r.v;
        p = r.p;
        return (Res){.v = -(v), .p = p};
    }
    return parseNumber(p);
}

int powInt(int base, int exp) {
    int r = 1;
    int b = base;
    int e = exp;
    while (e > 0) {
        if ((e % 2) == 1) {
            r = r * b;
        }
        b = b * b;
        e = e / (int)(2);
    }
    return r;
}

Res parsePower(Parser p) {
    Res r = parseFactor(p);
    int v = r.v;
    p = r.p;
    while (1) {
        p = skipWS(p);
        if ((p.pos < strlen(p.expr)) && (strcmp(_substring(p.expr, p.pos, p.pos + 1), "^") == 0)) {
            p.pos = p.pos + 1;
            Res r2 = parseFactor(p);
            int rhs = r2.v;
            p = r2.p;
            v = powInt(v, rhs);
        } else {
            break;
        }
    }
    return (Res){.v = v, .p = p};
}

Res parseTerm(Parser p) {
    Res r = parsePower(p);
    int v = r.v;
    p = r.p;
    while (1) {
        p = skipWS(p);
        if (p.pos < strlen(p.expr)) {
            const char* op = _substring(p.expr, p.pos, p.pos + 1);
            if (strcmp(op, "*") == 0) {
                p.pos = p.pos + 1;
                Res r2 = parsePower(p);
                int rhs = r2.v;
                p = r2.p;
                v = v * rhs;
                continue;
            }
            if (strcmp(op, "/") == 0) {
                p.pos = p.pos + 1;
                Res r2 = parsePower(p);
                int rhs = r2.v;
                p = r2.p;
                v = v / (int)(rhs);
                continue;
            }
        }
        break;
    }
    return (Res){.v = v, .p = p};
}

Res parseExpr(Parser p) {
    Res r = parseTerm(p);
    int v = r.v;
    p = r.p;
    while (1) {
        p = skipWS(p);
        if (p.pos < strlen(p.expr)) {
            const char* op = _substring(p.expr, p.pos, p.pos + 1);
            if (strcmp(op, "+") == 0) {
                p.pos = p.pos + 1;
                Res r2 = parseTerm(p);
                int rhs = r2.v;
                p = r2.p;
                v = v + rhs;
                continue;
            }
            if (strcmp(op, "-") == 0) {
                p.pos = p.pos + 1;
                Res r2 = parseTerm(p);
                int rhs = r2.v;
                p = r2.p;
                v = v - rhs;
                continue;
            }
        }
        break;
    }
    return (Res){.v = v, .p = p};
}

int evalExpr(const char* expr) {
    Parser p = (Parser){.expr = expr, .pos = 0};
    Res r = parseExpr(p);
    return r.v;
}

int user_main() {
    const char* expr = "2*(3-1)+2*5";
    puts(str_concat(str_concat(expr, " = "), str_int(evalExpr(expr))));
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
