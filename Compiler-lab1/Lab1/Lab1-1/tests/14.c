// funcdef: exp

void func()
{
    int a = 20;
    int b[a] = {0, 1, 2, 3, 4, 5, 6};
    int c = 2;
    
    b[c] = +10;
    b[c] = -b[3];
    b[c] = a + b[5];
    b[c] = a - b[5];
    b[c] = a * b[5];
    b[c] = a / b[5];
    b[c] = a % b[5];
    b[c] = (b[c+2]);
    
    b[c+3] = c * a + 0x12;
    b[c/2] = +0x12 * -0X13 + 1234567890 * a;
}