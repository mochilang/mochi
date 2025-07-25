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

typedef struct LDAPClient LDAPClient;

struct LDAPClient {
    const char* Base;
    const char* Host;
    int Port;
    int UseSSL;
    const char* BindDN;
    const char* BindPassword;
    const char* UserFilter;
    const char* GroupFilter;
    const char** Attributes;
    size_t Attributes_len;
};

int connect(LDAPClient client) {
    return (strcmp(client.Host, "") != 0) && (client.Port > 0);
}

int user_main() {
    LDAPClient client = (LDAPClient){.Base = "dc=example,dc=com", .Host = "ldap.example.com", .Port = 389, .UseSSL = 0, .BindDN = "uid=readonlyuser,ou=People,dc=example,dc=com", .BindPassword = "readonlypassword", .UserFilter = "(uid=%s)", .GroupFilter = "(memberUid=%s)", .Attributes = (const char*[]){ "givenName", "sn", "mail", "uid" }, .Attributes_len = 4};
    if (connect(client)) {
        puts(str_concat("Connected to ", client.Host));
    } else {
        puts("Failed to connect");
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
