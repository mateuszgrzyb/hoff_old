#include <stdio.h>

//extern double fibonacci(double);

extern double print(double d) {
    printf("%lf\n", d);
    return 0.0;
}

extern double read() {
    double d;
    scanf("%lf", &d);
    return d;
}

/*
int main() {
    //double d;
    printf("Enter double value:\n");
    //scanf("%lf", &d);
    double d = read();
    
    printf("calling hoff function fibonacci: ");
    print(d+1.0);
    printf("\n");
}
*/