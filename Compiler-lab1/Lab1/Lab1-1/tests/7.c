// funcdef: stmt: func

void a()
{}

void b()
{
    ;
}

void c()
{
    a();
    b();
    b(); a();
}