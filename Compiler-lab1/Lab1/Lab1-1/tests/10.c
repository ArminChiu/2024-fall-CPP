// funcdef: stmt: while

void func()
{
    int a;

    a = 24;
    while(a < 0x30)
        a = a + 1;
    
    while(a > 10)
    {
        a = a / 2;
    }
}