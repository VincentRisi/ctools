#ifndef _B20FORMS_H_
#define _B20FORMS_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <curses.h>
#include <time.h>

#include "ctosio.h"

typedef short int INT;
typedef unsigned char CHAR;
typedef CHAR* FORM;

#define FORM_BLOCK_SIGNATURE "B20FORM"
typedef struct b20_form_block
{
  char signature[8];
  char formname[40];
  ctos_form* form;
  WINDOW* win;
} b20_form_block;

typedef struct b20_type_code
{
  CHAR code[80];
}  b20_type_code, TYPE;

typedef struct b20_field_info 
{
  INT col
    , line
    , width
    , showDefault
    , autoExit
    , repeating
    , attrSel
    , attrUnsel
    , indexFirst
    , indexLast;
  CHAR reserved[10];
  INT defaultLen;
  CHAR defaultValue[80];
} b20_field_info, FIELDINFO;

typedef struct b20_init_state
{
  INT ich;        // initial cursor position 0 based;
  CHAR reserved[6];
} b20_init_state, INITSTATE;

typedef struct b20_exit_state
{
  INT ich         // initial cursor position 0 based;
    , ch          // terminating character code
    , autoExit    // TRUE if auto exit used
    , modified    // content changed
    , empty;      // empty on exit
  CHAR reserved[6];
} b20_exit_state, EXITSTATE;

#define MAX_TITLE         40
#define MAX_SYSID         20
typedef struct b20_thread_data
{
  bool draw;
  char title[MAX_TITLE + 1];
} b20_thread_data;

extern void Accept(INT* erc, INT row, INT col, CHAR* data, INT datalen, INT color, CHAR attr, INT* key, CHAR* filter, INT secret);
extern void AnyChar(INT* erc, CHAR *ch);
extern void AnyCode(INT* erc, CHAR *ch);
extern void Clear();
extern void CloseFile(INT* erc, INT file);
extern void CloseForm(INT* erc, CHAR* form);
extern void DateMailCo(CHAR* company_name, INT filenamelen);
extern void Defaultfield(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index);
extern void Defaultform(INT* erc, CHAR* form);
extern void Display(INT* erc, INT row, INT col, CHAR* data, INT datalen, INT color, CHAR attr, INT secret);
extern void Displayform(INT* erc, CHAR* form, INT frame, INT column, INT line);
extern void GetFieldInfo(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, FIELDINFO* fieldinfo, INT fieldinfomax); 
extern void LockKbd(INT* erc);
extern void OpenFile(INT* erc, INT* file, CHAR* filename, INT filenamelen, CHAR* password, INT passwordlen, INT mode);
extern void OpenForm(INT* erc, INT file, CHAR* formname, INT formnamelen, CHAR* form, INT max);
extern void ReadAll(INT* erc, CHAR* form, CHAR* block);
extern void ReadField(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index, CHAR* ret, INT max, INT* sizeret, TYPE* type);
extern void SetFieldAttrs(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index, CHAR attr);
extern void SetSysId(CHAR* sysid, INT sysidlen);
extern void UndisplayForm(INT* erc, CHAR* form);
extern void UserFillField(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index, INITSTATE* initstate, EXITSTATE* exitstate);
extern void WriteAll(INT* erc, CHAR* form, CHAR* block);
extern void WriteField(INT* erc, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index, CHAR* data, INT datalen, TYPE* type);
extern void WriteTo(INT* erc, CHAR* form, INT column, INT line, CHAR* data, INT datalen, INT color, CHAR attr);
extern void ZoomBox(INT* erc);
extern void Clone(INT* erc);
extern void Exec(INT* erc, CHAR* command, INT commandlen);
extern void SaveScreen();
extern void RestoreScreen();

#ifdef __cplusplus
};
#endif

#endif
