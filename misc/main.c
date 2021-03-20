#include <stdio.h>

extern double fibonacci(double);

int main() {
    double d;
    printf("Enter double value:\n");
    scanf("%lf", &d);
    
    printf("calling hoff function fibonacci: %lf\n", fibonacci(d));
}