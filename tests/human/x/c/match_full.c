// match_full.c - manual translation of tests/vm/valid/match_full.mochi
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

const char* classify(int n) {
    switch (n) {
        case 0: return "zero";
        case 1: return "one";
        default: return "many";
    }
}

int main() {
    int x = 2;
    const char *label;
    switch (x) {
        case 1: label = "one"; break;
        case 2: label = "two"; break;
        case 3: label = "three"; break;
        default: label = "unknown"; break;
    }
    printf("%s\n", label);

    const char *day = "sun";
    const char *mood;
    if (strcmp(day, "mon") == 0) mood = "tired";
    else if (strcmp(day, "fri") == 0) mood = "excited";
    else if (strcmp(day, "sun") == 0) mood = "relaxed";
    else mood = "normal";
    printf("%s\n", mood);

    bool ok = true;
    const char *status = ok ? "confirmed" : "denied";
    printf("%s\n", status);

    printf("%s\n", classify(0));
    printf("%s\n", classify(5));
    return 0;
}
