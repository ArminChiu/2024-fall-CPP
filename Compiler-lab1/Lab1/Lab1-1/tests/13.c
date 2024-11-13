// funcdef: exp

void func()
{
    int a = 20;
    int b[a] = {0, 1, 2, 3, 4, 5, 6};
    
    a = +10;
    a = -b[3];
    a = a + b[5];
    a = a - b[5];
    a = a * b[5];
    a = a / b[5];
    a = a % b[5];
    a = (a);
    
    int c;
    c = a;
    c = c * a + 0x12;
    c = 0x12 * 0X13 + 1234567890 * a;
}