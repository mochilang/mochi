// Generated by Mochi 0.10.32 on 2025-07-21 16:14 +0700
#include <stdio.h>
#include <string.h>
int x = 2;
const char *day = "sun";
int ok = 1;
const char *classify(int n) {
    return (n == 0 ? "zero" : (n == 1 ? "one" : "many"));
}
int main(void) {
    const char *label = (x == 1 ? "one" : (x == 2 ? "two" : (x == 3 ? "three" : "unknown")));
    puts(label);
    const char *mood = (strcmp(day, "mon") == 0 ? "tired" : (strcmp(day, "fri") == 0 ? "excited" : (strcmp(day, "sun") == 0 ? "relaxed" : "normal")));
    puts(mood);
    const char *status = (ok == 1 ? "confirmed" : "denied");
    puts(status);
    puts(classify(0));
    puts(classify(5));
    return 0;
}
