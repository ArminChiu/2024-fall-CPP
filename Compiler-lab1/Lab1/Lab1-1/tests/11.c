// funcdef: stmt: cond

void func()
{
    int a;
    int b[10] = {0, 1, 2, 3, 4, 5, 6};

    a = 3;
    if(a > 0)
        ;
    
    if(a < 0)
    {}

    if(a >= 1)
    {
        ;
    }

    if(a <= 2)
    {}
    else 
    {}
    
    if(a == 3)
    {
        a = 2 - a;
    }
    else {
        a = 2 + a;
    }

    if(a != 3)
        a = 3;
    else
        a = b[0];
}