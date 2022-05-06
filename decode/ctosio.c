#include "ctosio.h"
#include "binio.h"
#include "ctype.h"

ctos_form* ctos_alloc_form()
{
  ctos_form* form = (ctos_form*) malloc(sizeof(ctos_form));
  memset(form, 0, sizeof(ctos_form));
  return form;
}

ctos_field* ctos_alloc_field(ctos_form* form, const char* name)
{
  ctos_field* field;
  int i = form->no_fields;
  if (form->no_fields % 8 == 0)
    form->fields = (ctos_field*)realloc(form->fields, (i+8) * sizeof(ctos_field));
  field = &form->fields[i];
  memset(field, 0, sizeof(ctos_field));
  field->name = strdup(name);
  form->no_fields++;
  return field;
}

void ctos_add_line(ctos_form* form, const char* line)
{
  int i = form->no_lines;
  if (form->no_lines % 8 == 0)
  {
    size_t size = (i+8) * sizeof(char*);
    form->lines = (char**)realloc(form->lines, size);
  }
  form->lines[i] = strdup(line);
  form->no_lines++;
}

// *   0   *   0   *   3   *   0   *
// 0123456789012345678901234567890123
//           1         2         3
void ctos_check_color(ctos_form* form, const char* line)
{
  if (line[4] >= '0' && line[4] <= '7'
  &&  line[12] >= '0' && line[12] <= '3'
  &&  line[20] >= '0' && line[20] <= '3'
  &&  line[28] >= '0' && line[28] <= '3')
  {
    int color = line[4] - '0';
    form->colors[color][0] = line[12];
    form->colors[color][1] = line[20];
    form->colors[color][2] = line[28];
  }
}

void ctos_add_help(ctos_field* field, const char* line)
{
  int i = field->no_helps;
  if (field->no_helps % 8 == 0)
    field->help_message = (char**)realloc(field->help_message, (i+8) * sizeof(char*));
  field->help_message[i] = strdup(line);
  field->no_helps++;
}

void ctos_add_value(ctos_field* field, const char* value)
{
  int i = field->no_values;
  if (field->no_values % 8 == 0)
    field->list_of_values = (char**)realloc(field->list_of_values, (i+8) * sizeof(char*));
  field->list_of_values[i] = strdup(value);
  field->no_values++;
}

void ctos_free_form(ctos_form* form)
{
  int i;
  if (form->name != 0)
    free(form->name);
  if (form->no_lines > 0)
  {
    for (i = 0; i < form->no_lines; i++)
      free(form->lines[i]);
    free(form->lines);
  }
  if (form->data_size > 0)
    free(form->data);
  if (form->no_fields > 0)
  {
    for (i = 0; i < form->no_fields; i++)
      ctos_free_field(&form->fields[i]);
    free(form->fields);
  }
}

void ctos_free_field(ctos_field* field)
{
  int i;
  if (field->name != 0)
    free(field->name);
  if (field->default_value != 0)
    free(field->default_value);
  if (field->validation_routine != 0)
    free(field->validation_routine);
  if (field->no_helps > 0)
  {
    for (i = 0; i < field->no_helps; i++)
      free(field->help_message[i]);
    free(field->help_message);
  }
  if (field->no_values > 0)
  {
    for (i = 0; i < field->no_values; i++)
      free(field->list_of_values[i]);
    free(field->list_of_values);
  }
}

#define SIGNATURE "CTOS"
#define ARCHIVE   "ARCH"

void ctos_write_form(ctos_form* form, FILE *outfile)
{ 
  int i;
  bin_putstrn(SIGNATURE, 4, outfile);
  bin_putstr(form->name, outfile);
  bin_putshort(form->size, outfile);
  bin_putshort(form->height, outfile);
  bin_putshort(form->width, outfile);
  bin_putshort(form->number_of_fields, outfile);
  bin_putshort(form->data_size, outfile);
  bin_putshort(form->no_lines, outfile);
  for (i = 0; i < form->no_lines; i++)
    bin_putstr(form->lines[i], outfile);
  bin_putshort(form->no_fields, outfile);
  for (i = 0; i < form->no_fields; i++)
    ctos_write_field(&form->fields[i], outfile);
  bin_putshort(form->caption_color, outfile);
  bin_putshort(form->line_color, outfile);
  for (i = 0; i < 8; i++)
  {
    bin_putch(form->colors[i][0], outfile);
    bin_putch(form->colors[i][1], outfile);
    bin_putch(form->colors[i][2], outfile);
    bin_putch(form->colors[i][3], outfile);
  }
}

ctos_form* ctos_read_form(FILE *infile)
{
  int i;
  char signature[4];
  ctos_form* form;
  bin_getstrn(infile, signature, 4);
  if (strncmp(signature, SIGNATURE, 4) != 0)
    return 0;
  form = (ctos_form*) malloc(sizeof(ctos_form));
  form->name = bin_getstr(infile);
  form->size = bin_getshort(infile);
  form->height = bin_getshort(infile);
  form->width = bin_getshort(infile);
  form->number_of_fields = bin_getshort(infile);
  form->data_size = bin_getshort(infile);
  form->no_lines = bin_getshort(infile);
  form->lines = (char**) malloc(sizeof(char*)*form->no_lines);
  for (i = 0; i < form->no_lines; i++)
    form->lines[i] = bin_getstr(infile);
  form->no_fields = bin_getshort(infile);
  form->fields = (ctos_field*) malloc(form->no_fields * sizeof(ctos_field));
  for (i = 0; i < form->no_fields; i++)
    ctos_read_field(&form->fields[i], infile);
  form->caption_color = bin_getshort(infile);
  form->line_color = bin_getshort(infile);
  for (i = 0; i < 8; i++)
  {
    form->colors[i][0] = bin_getch(infile);
    form->colors[i][1] = bin_getch(infile);
    form->colors[i][2] = bin_getch(infile);
    form->colors[i][3] = bin_getch(infile);
  }
  if (form->data_size > 0)
    form->data = (char*) calloc(form->data_size, 1);
  return form;
}

static int _differs(const char *a, const char *b)
{
  int i, n=0;
  for (i=0; i<strlen(a) && n == 0; i++)
    n = toupper(a[i]) - toupper(b[i]);
  return n;
}

ctos_form* ctos_load_form(FILE *infile, const char* formname)
{
  char signature[4];
  long filesize, position, size;
  char* name;
  int n1, n2;
  n1 = strlen(formname);
  position = ftell(infile);
  bin_getstrn(infile, signature, 4);
  if (strncmp(signature, SIGNATURE, 4) == 0)
  {
    fseek(infile, position, SEEK_SET);
    return ctos_read_form(infile);
  }
  if (strncmp(signature, ARCHIVE, 4) != 0)
    return 0;
  fseek(infile, 0, SEEK_END);
  filesize = ftell(infile) - position;
  fseek(infile, position+4, SEEK_SET);
  while (1)
  {
    size = bin_getint(infile);
    position = ftell(infile);
    bin_getstrn(infile, signature, 4);
    if (strncmp(signature, SIGNATURE, 4) == 0)
    {
      name = bin_getstr(infile);
      n2 = strlen(name);
      if (n1 == n2)
      {
        if (_differs(formname, name) == 0)
        {
          free(name);
          fseek(infile, position, SEEK_SET);
          return ctos_read_form(infile);
        }
      }
      free(name);
      fseek(infile, position+size, SEEK_SET);
      if (ftell(infile) >= filesize)
        break;
    }
  }
  return 0;
}

void ctos_write_field(ctos_field* field, FILE *outfile)
{
  int i;
  bin_putstr(field->name, outfile);
  bin_putshort(field->row, outfile);
  bin_putshort(field->column, outfile);
  bin_putshort(field->width, outfile);
  bin_putshort(field->offset, outfile);
  bin_putch(field->repeating, outfile);
  bin_putshort(field->index, outfile);
  bin_putshort(field->first, outfile);
  bin_putshort(field->last, outfile);
  bin_putch(field->control, outfile);
  bin_putch(field->justification, outfile);
  bin_putshort(field->sequence_number, outfile);
  bin_putch(field->secret, outfile);
  bin_putch(field->prot, outfile);
  bin_putch(field->mand, outfile);
  bin_putstr(field->default_value, outfile);
  bin_putch(field->show_default, outfile);
  bin_putch(field->auto_exit, outfile);
  bin_putch(field->unselected, outfile);
  bin_putch(field->selected, outfile);
  bin_putstr(field->validation_routine, outfile);
  bin_putshort(field->no_helps, outfile);
  for (i = 0; i < field->no_helps; i++)
    bin_putstr(field->help_message[i], outfile);
  bin_putshort(field->selected_color, outfile);
  bin_putshort(field->unselected_color, outfile);
  bin_putshort(field->no_values, outfile);
  for (i = 0; i < field->no_values; i++)
    bin_putstr(field->list_of_values[i], outfile);
}

void ctos_read_field(ctos_field* field, FILE *infile)
{
  int i;
  field->name = bin_getstr(infile);
  field->row = bin_getshort(infile);
  field->column = bin_getshort(infile);
  field->width = bin_getshort(infile);
  field->offset = bin_getshort(infile);
  field->repeating = bin_getch(infile);
  field->index = bin_getshort(infile);
  field->first = bin_getshort(infile);
  field->last = bin_getshort(infile);
  field->control = bin_getch(infile);
  field->justification = bin_getch(infile);
  field->sequence_number = bin_getshort(infile);
  field->secret = bin_getch(infile);
  field->prot = bin_getch(infile);
  field->mand = bin_getch(infile);
  field->default_value = bin_getstr(infile);
  field->show_default = bin_getch(infile);
  field->auto_exit = bin_getch(infile);
  field->unselected = bin_getch(infile);
  field->selected = bin_getch(infile);
  field->validation_routine = bin_getstr(infile);
  field->no_helps = bin_getshort(infile);
  if (field->no_helps > 0)
  {
    field->help_message = (char**) malloc(sizeof(char*)*field->no_helps);
    for (i = 0; i < field->no_helps; i++)
      field->help_message[i] = bin_getstr(infile);
  }
  else
    field->help_message = 0;
  field->selected_color = bin_getshort(infile);
  field->unselected_color = bin_getshort(infile);
  field->no_values = bin_getshort(infile);
  if (field->no_values > 0)
  {
    field->list_of_values = (char**) malloc(sizeof(char*)*field->no_helps);
    for (i = 0; i < field->no_values; i++)
      field->list_of_values[i] = bin_getstr(infile);
  }
  else
    field->list_of_values = 0;
}

ctos_field* ctos_form_getfield(ctos_form* form, const char* name, short index)
{
  int i;
  ctos_field* no_result = 0;
  for (i = 0; i < form->no_fields; i++)
  {
    if (strcmp(form->fields[i].name, name) == 0)
    {
      if (form->fields[i].index == 0)
        return &form->fields[i];
      else if (form->fields[i].index == index)
        return &form->fields[i];
    }
  }
  return no_result;
}

void ctos_form_setfield_offset(ctos_form* form, ctos_field* field)
{
  ctos_field* one;
  if (field->repeating != 'Y')
  {
    form->data_size+=1;
    field->offset = form->data_size;
    form->data_size += field->width;
    return;
  }
  if (field->index == field->first)
  {
    form->data_size+=1;
    field->offset = form->data_size;
    form->data_size += field->width * ((field->last - field->first)+1);
    return;
  }
  one = ctos_form_getfield(form, field->name, 1);
  if (one == 0)
  {
    field->offset = form->data_size;
    form->data_size += field->width;
    return;
  }
  field->offset = one->offset + one->width * (field->index - one->index);
}

static char* _cob_name(const char* name, char* work, int worklen)
{
  int i, n;
  n = strlen(name);
  if (n >= worklen)
    return (char*) name;
  work[n] = 0;
  for (i = 0; i < n; i++)
  {
    work[i] = toupper(name[i]);
    if (work[i] < 'A' || work[i] > 'Z')
      if (work[i] < '0' || work[i] > '9')
        if (work[i] != '-')
          work[i]= '-';
  }
  return work;
}

void ctos_write_copybook(ctos_form* form, FILE *outfile)
{
  int i;
  char work1[256], work2[256], work3[256], work4[256];
  for (i = 0; i < form->no_lines; i++)
    fprintf(outfile
      , "      *> %s\n"
      , form->lines[i]
      );
  fprintf(outfile
      , 
"       01 DATA-%s.\n"
      , _cob_name(form->name, work1, sizeof(work1))
      );
  for (i = 0; i < form->no_fields; i++)
  {
    ctos_field* field = &form->fields[i];
    if (field->repeating == 'Y')
    {
      if (field->index > field->first)
        continue; 
      fprintf(outfile
        , 
"         03 D-CB%s BINARY-CHAR VALUE %d.\n"
        , _cob_name(field->name, work2, sizeof(work2))
        , field->width
        );
      fprintf(outfile
        , 
"         03 D-%s PIC X(%d) OCCURS %d.\n"
        , _cob_name(field->name, work2, sizeof(work2))
        , field->width
        , (field->last - field->first) + 1
        );
    }
    else
    {
      fprintf(outfile
        , 
"         03 D-CB%s BINARY-CHAR VALUE %d.\n"
        , _cob_name(field->name, work2, sizeof(work2))
        , field->width
        );
      fprintf(outfile
        , 
"         03 D-%s PIC X(%d).\n"
        , _cob_name(field->name, work2, sizeof(work2))
        , field->width)
        ;
    }
  }
  fprintf(outfile
      , 
"       01 DEFS-%s.\n"
      , _cob_name(form->name, work1, sizeof(work1))
      );
  fprintf(outfile
      , 
"         03 N-CBFORMNAME BINARY-CHAR VALUE %d.\n"
      , (int)strlen(form->name)
      );
  fprintf(outfile
      , 
"         03 N-FORMNAME PIC X(%d) VALUE '%s'.\n"
      , (int)strlen(form->name)
      , _cob_name(form->name, work2, sizeof(work2))
      );
  fprintf(outfile
      , 
"         03 N-FIELDS.\n"
      );
  int array_count = 0;    
  for (i = 0; i < form->no_fields; i++)
  {
    ctos_field* field = &form->fields[i];
    if (field->repeating == 'Y')
    {
      if (field->index > field->first)
        continue; 
      array_count++;  
      fprintf(outfile
        , 
"           05 N-CB%s BINARY-CHAR VALUE %d.\n"
        , _cob_name(field->name, work1, sizeof(work1))
        , (int)strlen(field->name)
        );
      fprintf(outfile
        , 
"           05 N-%s PIC X(40) VALUE '%s'.\n"
        , _cob_name(field->name, work1, sizeof(work1))
        //, strlen(field->name)
        , _cob_name(field->name, work2, sizeof(work2))
        );
      fprintf(outfile
        , 
"           05 N-OCC%s BINARY-CHAR VALUE %d.\n"
        , _cob_name(field->name, work1, sizeof(work1))
        , (field->last - field->first) + 1
        );
    }
    else
    {
      array_count++;  
      fprintf(outfile
        , 
"           05 N-CB%s BINARY-CHAR VALUE %d.\n"
        , _cob_name(field->name, work1, sizeof(work1))
        , (int)strlen(field->name)
        );
      fprintf(outfile
        , 
"           05 N-%s PIC X(40) VALUE '%s'.\n"
        , _cob_name(field->name, work1, sizeof(work1))
        //, strlen(field->name)
        , _cob_name(field->name, work2, sizeof(work2))
        );
      fprintf(outfile
        , 
"           05 N-OCC%s BINARY-CHAR VALUE 0.\n"
        , _cob_name(field->name, work1, sizeof(work1))
        );
    }
  }
  fprintf(outfile
    , 
"         03 N-FIELD-ARRAY REDEFINES N-FIELDS OCCURS %d.\n"
    , array_count
    );
  fprintf(outfile
    , 
"           05 N-CBFIELDNAME BINARY-CHAR.\n"
    );
  fprintf(outfile
    , 
"           05 N-FIELDNAME PIC X(40).\n"
    );
  fprintf(outfile
    , 
"           05 N-OCCFIELDNAME BINARY-CHAR.\n"
    );
}

static char* _cpp_name(const char* name, char* work, int worklen)
{
  int i, n;
  n = strlen(name);
  if (n >= worklen)
    return (char*) name;
  work[n] = 0;
  for (i = 0; i < n; i++)
  {
    work[i] = tolower(name[i]);
    if (work[i] < 'a' || work[i] > 'z')
      if (work[i] < '0' || work[i] > '9')
        if (work[i] != '_')
          work[i]= '_';
  }
  return work;
}

void ctos_write_cheader(ctos_form* form, FILE *outfile)
{
  int i;
  char work1[256];
  for (i = 0; i < form->no_lines; i++)
    fprintf(outfile
      , "/*  %s  */\n"
      , form->lines[i]);
  fprintf(outfile
    , "/* -------- %d ------- %d ------- */\n"
    , form->data_size
    , form->size
    );
  fprintf(outfile
    ,   "typedef struct ctos_%s\n{\n"
    , _cpp_name(form->name, work1, sizeof(work1))
    );
  for (i = 0; i < form->no_fields; i++)
  {
    ctos_field* field = &form->fields[i];
    if (field->repeating == 'Y')
    {
      if (field->index > field->first)
        continue;
      fprintf(outfile
        , "  unsigned char f_%s_len; // %d - %d\n"
        , _cpp_name(field->name, work1, sizeof(work1))
        , field->width
        , field->offset
        );
      fprintf(outfile
        , "  char f_%s[%d][%d];\n"
        , _cpp_name(field->name, work1, sizeof(work1))
        , (field->last - field->first) + 1
        , field->width
        );
    }
    else
    {
      fprintf(outfile
        , "  unsigned char f_%s_len; // %d - %d\n"
        , _cpp_name(field->name, work1, sizeof(work1))
        , field->width
        , field->offset
        );
      fprintf(outfile
        , "  char f_%s[%d];\n"
        , _cpp_name(field->name, work1, sizeof(work1))
        , field->width
        );
    }
  }
  fprintf(outfile
    , "} ctos_%s;\n\n"
    , _cpp_name(form->name, work1, sizeof(work1))\
    );
  fprintf(outfile
    ,   "static void set_ctos_%s_len(ctos_%s* this)\n{\n"
    , _cpp_name(form->name, work1, sizeof(work1))
    , _cpp_name(form->name, work1, sizeof(work1))
    );
  for (i = 0; i < form->no_fields; i++)
  {
    ctos_field* field = &form->fields[i];
    if (field->repeating == 'Y')
    {
      if (field->index > field->first)
        continue;
      fprintf(outfile
        , "  this->f_%s_len = %d; // %d\n"
        , _cpp_name(field->name, work1, sizeof(work1))
        , field->width
        , field->offset
        );
    }
    else
    {
      fprintf(outfile
        , "  this->f_%s_len = %d; // %d\n"
        , _cpp_name(field->name, work1, sizeof(work1))
        , field->width
        , field->offset
        );
    }
  }
  fprintf(outfile
    , "}\n"
    );
}
