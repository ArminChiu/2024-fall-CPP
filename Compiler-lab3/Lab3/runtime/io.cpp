#include <stdio.h>
#include <stdlib.h>
#include "io.h"

void input(int *i)
{
    printf("input:");
    scanf("%d", i);
}


void output(int *i)
{
    printf("output:%d\n", *i);
}

void obc_check_error(int *line, int *pos, char* str){
    //char* str = "obc array: [a]";
    printf("%s [Out-of-Bounds Check Error] at Line:%d, Pos:%d\n", str, *line, *pos);
    exit(-1);
}
