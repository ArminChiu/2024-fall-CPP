// funcdef: stmt: assign

void c()
{
    const int a = 3;
    const int b[] = {0, 1, 2, 3, 4};
    const int obc c[10] = {1, 2, 3, 4};
    
    int d;
    int e[] = {1, 2, 3};
    int obc f[0x10] = {0, 1, 2, 3, 4, -0, -1, -2};

    d = a;
    d = e[2];
    e[0] = e[1];
    e[1] = d;
}