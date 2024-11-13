// funcdef: stmt: if

void func()
{
    int a;

    a = 3;
    if(a > 0)
        ;
    
    if(a > 0)
    {}

    if(a > 1)
    {
        ;
        int x = 3;
    }

    if(a > 2)
    {}
    else 
    {}
    
    if(a < 2)
    {
        a = 2 - a;
    }
    else {
        a = 2 + a;
    }
}