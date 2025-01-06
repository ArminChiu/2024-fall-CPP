#ifndef _SAFEC_RUNTIME__IO_H
#define _SAFEC_RUNTIME__IO_H

#ifdef __cplusplus
extern "C" {
#endif

// Input a Number
void input(int *);
// Output a Number
void output(int *);

// Error report for obc error:
// obc_check_error(line_number, column_number, array_name);
void obc_check_error(int *,int *,char*);

#ifdef __cplusplus
}
#endif

#endif
