#ifndef __BINIO_H_
#define __BINIO_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

#ifdef __cplusplus
   extern "C" {
#endif

extern char bin_getch(FILE* binFile);
extern char* bin_getstr(FILE* binFile);
extern void bin_getstrn(FILE* binFile, char* s, int size);
extern short bin_getshort(FILE* binFile);
extern int bin_getint(FILE* binFile);
extern unsigned int bin_getutf(FILE* binFile);
extern unsigned int bin_getnum(FILE* binFile);
       
extern void bin_putch(char c, FILE* binFile);
extern void bin_putstr(char* s, FILE* binFile);
extern void bin_putstrn(char* s, int size, FILE* binFile);
extern void bin_putshort(short i, FILE* binFile);
extern void bin_putint(int i, FILE* binFile);
extern void bin_pututf(unsigned int u, FILE* binFile);
extern void bin_putnum(unsigned int u, FILE* binFile);

#ifdef __cplusplus
   };
#endif

#endif

