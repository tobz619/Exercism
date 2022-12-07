#include "armstrong_numbers.h"
#include <stdio.h>
#include <math.h>

bool is_armstrong_number(int number)
{
    bool def = false;
    int acc = 0;
    int numOfDigits = 0;
    int copyNumber = number;
    int copyNumber2 = number;
    while (number > 0)
    {
        numOfDigits++;
        copyNumber = copyNumber / 10;
    }
    for (int i = 0; i < numOfDigits; i++)
    {
        acc += pow(copyNumber2, numOfDigits);
        copyNumber2 = copyNumber2 / 10;
    }
    if (acc == number)
    {
        def = true;
    }
    return def;
}