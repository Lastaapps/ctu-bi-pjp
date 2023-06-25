#include<stdint.h>
#include<stdio.h>

int64_t built_in_read_line_integer(int64_t* ptr) {
    return scanf("%ld", ptr);
}
int64_t built_in_read_line_float(double* ptr) {
    return scanf("%lf", ptr);
}

int64_t built_in_write_string(char* ptr) {
    int ret = printf("%s", ptr);
    fflush(stdin);
    return ret;
}

int64_t built_in_write_integer(int64_t val) {
    int ret = printf("%ld", val);
    fflush(stdin);
    return ret;
}

int64_t built_in_write_float(double val) {
    int ret = printf("%lf", val);
    fflush(stdin);
    return ret;
}
