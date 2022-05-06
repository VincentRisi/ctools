#ifndef _CTOSIO_H_
#define _CTOSIO_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

// unselected and selected - letter meanings
// letters used - A..P -- Blinking ABCDEFGH no IJKLMNOP yes | 0x08 | ie. no = letter - 'A'
//                        Reverse  ABCDIJKL no EFGHMNOP yes | 0x04 |
//                      Underline  ABEFIJMN no CDGHKLOP yes | 0x02 |
//                    Half Bright  ACEGIKMO no BDFHJLNP yes | 0x01 |
//
// field name max 40 characters

typedef struct ctos_field
{
  char* name;
  short row, column, width, offset;
  char repeating;
  short index, first, last;
  char control;
  char justification;
  short sequence_number;
  char secret, prot, mand;
  char* default_value;
  char show_default, auto_exit;
  char unselected, selected;
  char* validation_routine;
  short no_helps;
  char* *help_message;
  short selected_color, unselected_color;
  short no_values;
  char* *list_of_values;
} ctos_field;

typedef struct ctos_form
{
  char* name;
  short size, height, width, number_of_fields, data_size;
  short no_lines;
  char* *lines;
  short no_fields;
  ctos_field *fields;
  short caption_color, line_color;
  char colors[8][4];
  char* data;
} ctos_form;

#ifdef __cplusplus
   extern "C" {
#endif

extern ctos_form* ctos_alloc_form();
extern void ctos_add_line(ctos_form* form, const char* line);
extern void ctos_check_color(ctos_form* form, const char* line);
extern void ctos_free_form(ctos_form* form);
extern void ctos_write_form(ctos_form* form, FILE *outfile);
extern void ctos_write_copybook(ctos_form* form, FILE *outfile);
extern void ctos_write_cheader(ctos_form* form, FILE *outfile);
extern ctos_form* ctos_read_form(FILE *infile);
extern ctos_form* ctos_load_form(FILE *infile, const char* formname);
extern void ctos_display_form(ctos_form* form, void *formdata);
extern ctos_field* ctos_form_getfield(ctos_form* form, const char* name, short index);
extern void ctos_form_setfield_offset(ctos_form* form, ctos_field* field);

extern ctos_field* ctos_alloc_field(ctos_form* form, const char* name);
extern void ctos_add_help(ctos_field* field, const char* line);
extern void ctos_add_value(ctos_field* field, const char* value);
extern void ctos_free_field(ctos_field* field);
extern void ctos_write_field(ctos_field* field, FILE *outfile);
extern void ctos_read_field(ctos_field* field, FILE *infile);

#ifdef __cplusplus
   };
#endif


#endif