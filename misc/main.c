#include <stdio.h>

extern double something(double);

int main() {
    double d;
    printf("Enter double value:\n");
    scanf("%lf", &d);
    printf("calling hoff function something: %lf\n", something(d));
}