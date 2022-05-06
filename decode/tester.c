#include "tester.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "b20forms.h"
#include "strelomt.h"
#include "curses.h"

static char* format(char* work, int len, const char *fmt, ...)
{
  va_list  args;
  va_start(args, fmt);
  vsnprintf(work, len+1, fmt, args);
  va_end(args);
  return work;
}

typedef struct Fx
{
  CHAR* name;
  INT len;
  int occurs;
} FX;

FX fx[] = 
{ {(CHAR*)"TRANSCODE",      9,  0}    //    0  1
, {(CHAR*)"TRANSDESC",      9,  0}    // 1  2  3  4  5  6  7
, {(CHAR*)"STOCKNUMBER",   11, 10}    // 2  8  9 10 11 12 13
, {(CHAR*)"QUANTITY",       8, 10}    // 3 14 15 16 17 18 19
, {(CHAR*)"UNITPRICE",      9, 10}    // 4 20 21 22 23 24 25
, {(CHAR*)"TOTALPRICE",    10, 10}    // 5 26 27 28 29 30 31
, {(CHAR*)"REFERENCENO",   11, 10}    // 6 32 33 34 35 36 37
, {(CHAR*)"REFERENCEDATE", 13, 10}    // 7 38 39 40 41 42 43
, {(CHAR*)"DESCRIPTION1",  12,  0}    // 8 44 45 46 47 48 49
, {(CHAR*)"DESCRIPTION2",  12,  0}    // 9 50 51 52 53 54 55
};                                    //10 56 57 58 59 60 61
                                      //   62 63
CHAR input_test(INT* erc, CHAR *form) 
{
  int no = 0, fxno, index;
  INITSTATE initstate;
  EXITSTATE exitstate;
  while (1)
  {
    if (no < 2)
    {
      fxno = no;
      index = 0;
    }
    else if (no < 62)  
    {
      fxno =  ((no-2) % 6) + 2;
      index = ((no-2) / 6) + 1;
    }
    else
    { 
      fxno = (no - 63) + 8;
      index = 0;
    }
    initstate.ich = 0;
    UserFillField(erc, form, fx[fxno].name, fx[fxno].len, index, &initstate, &exitstate);
    switch (exitstate.ch)
    {
    case 0x01: // UP
      if (no > 0)
        no--;
      break;  
    case 0x0b: // DOWN  
    case 0x09: // tab
    case 0x12: // auto_exit RIGHT
      if (no < 64)
        no++;
      break;  
    default:
      return exitstate.ch;    
    }
  }
}

void printout(const char* descr, const char* value, int len)
{
  printf("%s %*.*s\n", descr, len, len, value);
}

int main(int argc, char* argv[])
{
  INT erc, file, i, key, ch, state=0;
  char work[256];
  CHAR filename[] = "/ctools/dev/build/data/CoForms.Lib";
  CHAR formname[]  = "StReLoMt";
  CHAR form2name[] = "StDescIq";
  CHAR password[] = "";
  CHAR form[100];
  CHAR form2[100];
  CHAR exitcode;
  CHAR title[] = "**WINGDING SPRINGER SLAPSTICK**";
  CHAR sysid[] = "FREDDY";
  CHAR *filter = "\x0A\x1B\x04\x01\x0B\x07\x0C\x1D\x1F\x00";
  CHAR cbret[41]; 
  INT  cbret_len;
  TYPE type; 
  initscr();
  strcpy((char*)type.code, "CHARACTER.");
  ctos_strelomt strelomt;
  set_ctos_strelomt_len(&strelomt);
  DateMailCo(title, strlen(title));
  SetSysId(sysid, strlen(sysid));
  OpenFile(&erc, &file, filename, (INT)strlen((char*)filename), password, 0, (INT)0x6D72);
  OpenForm(&erc, file, formname, (INT)strlen((char*)formname), form, 100);
  OpenForm(&erc, file, form2name, (INT)strlen((char*)form2name), form2, 100);
  CloseFile(&erc, file);
  Displayform(&erc, form, 0, 1, 1);
  strncpy(strelomt.f_transcode,        "A",                    strelomt.f_transcode_len);
  strncpy(strelomt.f_transdesc,        "BERTHA              ", strelomt.f_transdesc_len);
  for (i = 1; i <= 10; i++)
  {
    int n=i-1;
    strncpy(strelomt.f_stocknumber[n],   format(work, 15, "STOCK %-9d", i), strelomt.f_stocknumber_len);
    strncpy(strelomt.f_quantity[n],      format(work,  6, "%6d", i), strelomt.f_quantity_len);
    strncpy(strelomt.f_unitprice[n],     format(work, 11, "%11.2f", i+13.17), strelomt.f_unitprice_len);
    strncpy(strelomt.f_totalprice[n],    format(work, 11, "%11.2f", i*(i+13.17)), strelomt.f_totalprice_len);
    strncpy(strelomt.f_referenceno[n],   format(work, 20, "REF NO %-13d", i), strelomt.f_referenceno_len);
    strncpy(strelomt.f_referencedate[n], format(work, 10, "2014/06/%02d", i), strelomt.f_referencedate_len);
  }                              //012345678901234567890
  strncpy(strelomt.f_description1, "DESCRIPTION ONE     ", strelomt.f_description1_len);
  strncpy(strelomt.f_description2, "DESCRIPTION TWO     ", strelomt.f_description2_len);
  WriteAll(&erc, form, (CHAR*)&strelomt);
  exitcode = input_test(&erc, form);
  #define NO_TEST_ZOOMBOX
  #if defined(TEST_ZOOMBOX)
  ZoomBox(&erc);
  if (erc != 0)
  {
    CloseForm(&erc, form);
    Display(&erc, 25, 12, title, strlen(title), 2, 'C', 0);
    Accept(&erc, 25, strlen(title)+14, formname, strlen(formname), 2, 'E', &key, filter, 0);
  	exit(0);
  }
  #endif
  #define NO_TEST_CLONE
  #if defined(TEST_CLONE)
  Clone(&erc);
  if (erc != 0)
  {
    CloseForm(&erc, form);
    Display(&erc, 25, 12, title, strlen(title), 2, 'C', 0);
    Accept(&erc, 25, strlen(title)+14, formname, strlen(formname), 2, 'E', &key, filter, 0);
    exit(0);
  }
  #endif
  #define NO_TEST_EXEC
  #if defined(TEST_EXEC)
  #define EXEC_COMMAND "bash"
  Exec(&erc, EXEC_COMMAND, sizeof(EXEC_COMMAND));
  #endif
  #define TEST_SAVE_AND_RESTORE
  #if defined(TEST_SAVE_AND_RESTORE)
  SaveScreen();
  Display(&erc, 5, 12, title, strlen(title), 2, 'C', 0);
  Accept(&erc, 5, strlen(title)+14, formname, strlen(formname), 2, 'E', &key, filter, 0);
  RestoreScreen();
  #endif
  exitcode = input_test(&erc, form);
  ReadField(&erc, form, "TRANSDESC", 9, 0, cbret, 40, &cbret_len, &type);
  ReadAll(&erc, form, (CHAR*)&strelomt);
  CloseForm(&erc, form);
  Display(&erc, 5, 12, title, strlen(title), 2, 'C', 0);
  Accept(&erc, 5, strlen(title)+14, formname, strlen(formname), 2, 'E', &key, filter, 0);
  Display(&erc, 5, 12, title, strlen(title), 2, 'C', 0);
  Accept(&erc, 5, strlen(title)+14, formname, strlen(formname), 2, 'E', &key, filter, 1);
  endwin();
  CHAR buffer[256];
  INT ret;
  printout("TRANSCODE", strelomt.f_transcode, strelomt.f_transcode_len);
  printout("TRANSDESC", strelomt.f_transdesc, strelomt.f_transdesc_len);
  for (i = 1; i <= 10; i++)
  {
    int n=i-1;
    printout("STOCKNUMBER  ", strelomt.f_stocknumber[n],   strelomt.f_stocknumber_len);
    printout("QUANTITY     ", strelomt.f_quantity[n],      strelomt.f_quantity_len);
    printout("UNITPRICE    ", strelomt.f_unitprice[n],     strelomt.f_unitprice_len);
    printout("TOTALPRICE   ", strelomt.f_totalprice[n],    strelomt.f_totalprice_len);
    printout("REFERENCENO  ", strelomt.f_referenceno[n],   strelomt.f_referenceno_len);
    printout("REFERENCEDATE", strelomt.f_referencedate[n], strelomt.f_referencedate_len);
  }
  printout("DESCRIPTION1", strelomt.f_description1, strelomt.f_description1_len);
  printout("DESCRIPTION2", strelomt.f_description2, strelomt.f_description2_len);
  printf("exitcode(%x)\n", exitcode);  
  printf("%s -> %d\n", formname, key);
  printf("[%s] size %d\n", cbret, (int)cbret_len);
  return 0;
}

