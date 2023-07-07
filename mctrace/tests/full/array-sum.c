#include <stdio.h>
#include <stdint.h>

uint32_t array_sum(uint32_t* arr, uint32_t len) {
    uint32_t total = 0;
    for(uint32_t i = 0; i < len ; ++i)
        total += arr[i] ;
    return total;
}

int main() {
    uint32_t arr[] = { 4, 3, 2, 1 };
    uint32_t sum = array_sum(arr, 4);
    printf("Sum = %d\n", sum);
}
