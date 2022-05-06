#include "binio.h"

#define BINIO_USE_SWAP
#if defined(BINIO_USE_SWAP)
static void _swap(char *a, char *b) 
{ 
  char temp; temp = *a; *a = *b; *b = temp; 
}

const int doSwap=0x03020100;

static void swapshort(short *no, int size)
{
  if (*(char*)&doSwap == 0) // seen as 0x00010203 (perhaps)
  {
    char* p = (char*)no;
    _swap(&p[0], &p[1]);
  }
}  

static void swapint(int *no, int size)
{
  if (*(char*)&doSwap == 0) // seen as 0x00010203 (perhaps)
  {
    char* p = (char*)no;
    _swap(&p[0], &p[3]);
    _swap(&p[1], &p[2]);
  }
}
#endif
char bin_getch(FILE* binFile)
{
  return fgetc(binFile);
}

char* bin_getstr(FILE* binFile)
{
  int n;
  char* s = 0;
  n = fgetc(binFile);
  if (n > 0)
  {
    s = (char*)calloc(n + 1, 1);
    fread(s, 1, n, binFile);
  }
  return s;
}

void bin_getstrn(FILE* binFile, char* s, int size)
{
  fread(s, 1, size, binFile);
}

short bin_getshort(FILE* binFile)
{
#if defined(BINIO_USE_SWAP)
  short u;
  fread(&u, sizeof(short), 1, binFile);
  swapshort(&u, (int)sizeof(u));
  return u;
#else
  unsigned int u;
  u = bin_getutf(binFile);
  return (short) u;
#endif
}

int bin_getint(FILE* binFile)
{
#if defined(BINIO_USE_SWAP)
  int u;
  fread(&u, sizeof(int), 1, binFile);
  swapint(&u, (int)sizeof(u));
  return u;
#else
  unsigned int u = bin_getutf(binFile);
  return (int) u;
#endif
}

// U-00000000 U-0000007F: 0xxxxxxx
// U-00000080 U-000007FF: 110xxxxx 10xxxxxx
// U-00000800 U-0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
// U-00010000 U-001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
// U-00200000 U-03FFFFFF: 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
// U-04000000 U-7FFFFFFF: 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx

unsigned int bin_getutf(FILE* binFile)
{
  unsigned char u[7], utf;
  unsigned int result;
  fread(&u[0], 1, 1, binFile);
  utf = u[0] & 0xFE;
  if ((utf & 0x80) == 0) 
  {
    result = u[0];
    return result;
  }
  fread(&u[1], 1, 1, binFile);
  if ((utf & 0xE0) == 0xC0)
  {
    result = (((unsigned int)u[0] & 0x1F) << 6) | ((unsigned int)u[1] & 0x3F);
    return result;
  }
  fread(&u[2], 1, 1, binFile);
  if ((utf & 0xF0) == 0xE0)
  {
    result = (((unsigned int)u[0] & 0x0F) << 12) | (((unsigned int)u[1] & 0x3F) << 6) | ((unsigned int)u[2] & 0x3F);
    return result;
  }
  fread(&u[3], 1, 1, binFile);
  if ((utf & 0xF8) == 0xF0)
  {
    result = (((unsigned int)u[0] & 0x07) << 18) | (((unsigned int)u[1] & 0x3F) << 12) | (((unsigned int)u[2] & 0x3F) << 6) | ((unsigned int)u[3] & 0x3F);
    return result;
  }
  fread(&u[4], 1, 1, binFile);
  if ((utf & 0xFC) == 0xF8)
  {
    result = (((unsigned int)u[0] & 0x03) << 24) | (((unsigned int)u[1] & 0x3F) << 18) | (((unsigned int)u[2] & 0x3F) << 12) | (((unsigned int)u[3] & 0x3F) << 6) | ((unsigned int)u[4] & 0x3F);
    return result;
  }
  fread(&u[5], 1, 1, binFile);
  if ((utf & 0xFE) == 0xFC)
  {
    result = (((unsigned int)u[0] & 0x01) << 30) | (((unsigned int)u[1] & 0x3F) << 24) | (((unsigned int)u[2] & 0x3F) << 18) | (((unsigned int)u[3] & 0x3F) << 12) | (((unsigned int)u[4] & 0x3F) << 6) | ((unsigned int)u[5] & 0x3F);
    return result;
  }
  fread(&u[6], 1, 1, binFile);
  result = (((unsigned int)u[1] & 0x03) << 30) | (((unsigned int)u[2] & 0x3F) << 24) | (((unsigned int)u[3] & 0x3F) << 18) | (((unsigned int)u[4] & 0x3F) << 12) | (((unsigned int)u[5] & 0x3F) << 6) | ((unsigned int)u[6] & 0x3F);
  return result;
}

// U-00000000 – U-0000007F: 0xxxxxxx
// U-00000080 – U-00003FFF: 10xxxxxx xxxxxxxx
// U-00004000 – U-001FFFFF: 110xxxxx xxxxxxxx xxxxxxxx
// U-00200000 – U-0FFFFFFF: 1110xxxx xxxxxxxx xxxxxxxx xxxxxxxx
// U-10000000 – U-FFFFFFFF: 11110000 xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx

unsigned int bin_getnum(FILE* binFile)
{
  unsigned char u[5], num;
  unsigned int result;
  fread(&u[0], 1, 1, binFile);
  num = u[0] & 0xF1;
  if ((num & 0x80) == 0) 
  {
    result = u[0];
    return result;
  }
  fread(&u[1], 1, 1, binFile);
  if ((num & 0xC0) == 0x80)
  {
    result = (((unsigned int)u[0] & 0x3F) << 8) | (unsigned int)u[1];
    return result;
  }
  fread(&u[2], 1, 1, binFile);
  if ((num & 0xE0) == 0xC0)
  {
    result = (((unsigned int)u[0] & 0x1F) << 16) | ((unsigned int)u[1] << 8) | (unsigned int)u[2];
    return result;
  }
  fread(&u[3], 1, 1, binFile);
  if ((num & 0xF0) == 0xE0)
  {
    result = (((unsigned int)u[0] & 0x0F) << 24) | ((unsigned int)u[1] << 16) | ((unsigned int)u[2] << 8)| (unsigned int)u[3];
    return result;
  }
  fread(&u[4], 1, 1, binFile);
  result = ((unsigned int)u[1] << 24) | ((unsigned int)u[2] << 16) | ((unsigned int)u[3] << 8)| (unsigned int)u[4];
  return result;
}

void bin_putch(char c, FILE* binFile)
{
  fputc(c, binFile);
}

void bin_putstr(char* s, FILE* binFile)
{
  if (s != 0)
  {
    fputc((unsigned char)strlen(s), binFile);
    fputs(s, binFile);
  }
  else
    fputc(0, binFile);
}

void bin_putstrn(char* s, int size, FILE* binFile)
{
  fwrite(s, 1, size, binFile);
}

void bin_putshort(short i, FILE* binFile)
{
#if defined(BINIO_USE_SWAP)
  swapshort(&i, (int)sizeof(i));
  fwrite(&i, sizeof(short), 1, binFile);
#else
  bin_pututf((unsigned int)i, binFile);
#endif
}

void bin_putint(int i, FILE* binFile)
{
#if defined(BINIO_USE_SWAP)
  swapint(&i, (int)sizeof(i));
  fwrite(&i, sizeof(int), 1, binFile);
#else
  bin_pututf((unsigned int)i, binFile);
#endif
}

#define UTF1 0x0000007F
#define UTF2 0x000007FF
#define UTF3 0x0000FFFF
#define UTF4 0x001FFFFF
#define UTF5 0x03FFFFFF
#define UTF6 0x7FFFFFFF             

void bin_pututf(unsigned int i, FILE* binFile)
{
  unsigned int u = (unsigned int)i;
  unsigned char n[7];
  if (u <= UTF1) // U-00000000 U-0000007F: 0xxxxxxx
  {
    n[0]=u;
    fwrite(n, 1, 1, binFile);                                              
  }                                                                        
  else if (u <= UTF2) // U-00000080 U-000007FF: 110xxxxx 10xxxxxx
  {                                                                        
    n[0]=(((u & 0x000003C0) >>  6) & 0x1F) | 0xC0;                         
    n[1]=(((u & 0x0000003F)      ) & 0x3F) | 0x80;                         
    fwrite(n, 1, 2, binFile);                                              
  }                                                                        
  else if (u <= UTF3) // U-00000800 U-0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx                                                     
  {
    n[0]=(((u & 0x0000F000) >> 12) & 0x0F) | 0xE0;                         
    n[1]=(((u & 0x00000FC0) >>  6) & 0x3F) | 0x80;                         
    n[2]=(((u & 0x0000003F)      ) & 0x3F) | 0x80;                         
    fwrite(n, 1, 3, binFile);                                              
  }
  else if (u <= UTF4) // U-00010000 U-001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx                                                                     
  { 
    n[0]=(((u & 0x001C0000) >> 18) & 0x07) | 0xF0;
    n[1]=(((u & 0x0003F000) >> 12) & 0x3F) | 0x80; 
    n[2]=(((u & 0x00000FC0) >>  6) & 0x3F) | 0x80; 
    n[3]=(((u & 0x0000003F)      ) & 0x3F) | 0x80; 
    fwrite(n, 1, 4, binFile);
  }
  else if (u <= UTF5) // U-00200000 U-03FFFFFF: 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
  {
    n[0]=(((u & 0x40000000) >> 24) & 0x03) | 0xF8;
    n[1]=(((u & 0x00FC0000) >> 18) & 0x3F) | 0x80; 
    n[2]=(((u & 0x0003F000) >> 12) & 0x3F) | 0x80; 
    n[3]=(((u & 0x00000FC0) >>  6) & 0x3F) | 0x80; 
    n[4]=(((u & 0x0000003F)      ) & 0x3F) | 0x80; 
    fwrite(n, 1, 5, binFile);
  }
  else if (u <= UTF6) // U-04000000 U-7FFFFFFF: 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
  {
    n[0]=(((u & 0x40000000) >> 30) & 0x01) | 0xFC;
    n[1]=(((u & 0x3F000000) >> 24) & 0x3F) | 0x80; 
    n[2]=(((u & 0x00FC0000) >> 18) & 0x3F) | 0x80; 
    n[3]=(((u & 0x0003F000) >> 12) & 0x3F) | 0x80; 
    n[4]=(((u & 0x00000FC0) >>  6) & 0x3F) | 0x80; 
    n[5]=(((u & 0x0000003F)      ) & 0x3F) | 0x80; 
    fwrite(n, 1, 6, binFile);
  }
  else // U-04000000 U-FFFFFFFF: 11111110 1000000xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
  {
    n[0]=0xFE;
    n[1]=(((u & 0xC0000000) >> 30) & 0x3F) | 0x80; 
    n[2]=(((u & 0x3F000000) >> 24) & 0x3F) | 0x80; 
    n[3]=(((u & 0x00FC0000) >> 18) & 0x3F) | 0x80; 
    n[4]=(((u & 0x0003F000) >> 12) & 0x3F) | 0x80; 
    n[5]=(((u & 0x00000FC0) >>  6) & 0x3F) | 0x80; 
    n[6]=(((u & 0x0000003F)      ) & 0x3F) | 0x80; 
    fwrite(n, 1, 7, binFile);
  }
}

#define NUM1 0x0000007F
#define NUM2 0x00003FFF
#define NUM3 0x001FFFFF
#define NUM4 0x0FFFFFFF
#define NUM5 0xFFFFFFFF

void bin_putnum(unsigned int i, FILE* binFile)
{
  unsigned int u = (unsigned int)i;
  unsigned char n[7];
  if (u <= NUM1) // U-00000000 – U-0000007F: 0xxxxxxx
  {
    n[0]=u;
    fwrite(n, 1, 1, binFile);                                              
  }                                                                        
  else if (u <= NUM2) // U-00000080 – U-00003FFF: 10xxxxxx xxxxxxxx
  {                                                                        
    n[0]=(u & 0x00003F00) | 0x80;                         
    n[1]=(u & 0x000000FF);                         
    fwrite(n, 1, 2, binFile);                                              
  }                                                                        
  else if (u <= NUM3) // U-00004000 – U-0000FFFF: 1100xxxx xxxxxxxx xxxxxxxx                                                     
  {
    n[0]=((u & 0x001F0000) >> 16) | 0xC0;                         
    n[1]= (u & 0x0000FF00) >>  8;                         
    n[2]= (u & 0x000000FF);                         
    fwrite(n, 1, 3, binFile);                                              
  }
  else if (u <= NUM4) // U-00010000 – U-001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx                                                                     
  { 
    n[0]=((u & 0x0F000000) >> 24) | 0xF0;
    n[1]= (u & 0x00FF0000) >> 16; 
    n[2]= (u & 0x0000FF00) >>  8; 
    n[3]= (u & 0x000000FF); 
    fwrite(n, 1, 4, binFile);
  }
  else
  {
    n[0]=0xF8;
    n[1]=(u & 0xFF000000) >> 24; 
    n[2]=(u & 0x00FF0000) >> 16; 
    n[3]=(u & 0x0000FF00) >>  8; 
    n[4]=(u & 0x000000FF); 
    fwrite(n, 1, 5, binFile);
  }
}
