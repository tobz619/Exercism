#include "armstrong_numbers.h"
#include <stdio.h>
#include <math.h>

int raisepower(int num, int raise)
{
    int increase = num;
    while (raise > 1)
    {
        increase = increase * num;
        raise--;
    }
    return increase;
}

bool is_armstrong_number(int number)
{
    bool def = false;
    int acc = 0;
    int rem;
    int temp1 = number;
    int temp2 = number;
    int numOfDigits = 0;
    while (temp1 > 0)
    {
        // printf("temp = %d; numDigs = %d; \n", temp1, numOfDigits);
        numOfDigits++;
        temp1 = temp1 / 10;
    }
    while (temp2 > 0)
    {
        // printf("Number = %d; acc = %d; rem  = %d \n", temp2, acc, rem);
        rem = temp2 % 10;
        acc = acc + raisepower(rem, numOfDigits);
        temp2 = temp2 / 10;
    }
    if (acc == number)
    {
        def = true;
    }
    return def;
}