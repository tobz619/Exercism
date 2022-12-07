#include "armstrong_numbers.h"
#include <stdio.h>
#include <math.h>

<<<<<<< HEAD
=======
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

>>>>>>> 2c2be32eab6f16b7745ac2cacbe20696e42e8a25
bool is_armstrong_number(int number)
{
    bool def = false;
    int acc = 0;
<<<<<<< HEAD
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
=======
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
>>>>>>> 2c2be32eab6f16b7745ac2cacbe20696e42e8a25
    }
    if (acc == number)
    {
        def = true;
    }
    return def;
}