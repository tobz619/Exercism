#include "armstrong_numbers.h"
#include <stdio.h>
#include <math.h>

bool is_armstrong_number(int number)
{
    bool def = false;
    int acc = 0;
    int rem;
    int *copyNumber = &number;
    while (*copyNumber > 0)
    {
        rem = *copyNumber % 10;
        acc = acc + (rem * rem * rem);
        *copyNumber = *copyNumber / 10;
        printf("Number = %d; copyNumber = %d \n", number, *copyNumber);
    }
    if (acc == number)
    {
        def = true;
    }
    return def;
}