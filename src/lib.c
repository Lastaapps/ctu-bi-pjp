#include<stdint.h>
#include<stdio.h>

int64_t built_read_line(int64_t* ptr) {
    return scanf("%ld", ptr);
}

int64_t built_write(char* ptr) {
    return printf("%s", ptr);
}

int64_t built_print(int64_t val) {
    return printf("%ld", val);
}
