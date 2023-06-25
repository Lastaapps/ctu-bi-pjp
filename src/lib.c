#include<stdint.h>
#include<stdio.h>

int64_t built_read_line_integer(int64_t* ptr) {
    return scanf("%ld", ptr);
}
int64_t built_read_line_float(double* ptr) {
    return scanf("%lf", ptr);
}

int64_t built_write(char* ptr) {
    return printf("%s", ptr);
}

int64_t built_print_integer(int64_t val) {
    return printf("%ld", val);
}

int64_t built_print_float(double val) {
    return printf("%lf", val);
}
