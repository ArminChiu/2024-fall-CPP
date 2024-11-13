const int a = 3;
const int b = 4, c = +5, d = 1+9;
const int e[a] = {1, 2, 3};
const int f[] = {3, 0x13, -0x19}, obc g[] = {100, 101, 102, 103, 104};

void func()
{
    int x = a * b + c % (d + e[b-3] * f[1]) - g[3];
    int y[] = {a + e[0], f[1] + g[1]};
}
