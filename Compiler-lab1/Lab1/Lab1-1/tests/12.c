// funcdef: stmt: cond

void func()
{
    int a;
    int b[10] = {0, 1, 2, 3, 4, 5, 6};
    
    b[5] = 3;
    if(b[5] > 0)
        ;
    
    if(b[5] < 0)
    {}

    if(b[5] >= 1)
    {
        ;
    }

    if(b[5] <= 2)
    {}
    else 
    {}
    
    if(b[5] == b[6])
    {
        b[5] = 2 - b[5];
    }
    else {
        b[5] = 2 + b[5];
    }

    if(b[5] != b[2])
        b[5] = 3;
    else
        b[5] = b[6];
}