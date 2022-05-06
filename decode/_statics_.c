#ifndef _statics_c_
#define _statics_c_

static void       _accept_edit_(INT* erc, WINDOW* win, int row, int col, CHAR* data, INT datalen, CHAR attr, int color, INT* key, CHAR* filter, int secret);
static void       _accept_write_(INT* erc, WINDOW* win, int row, int col, CHAR* data, INT datalen, char attr, int color, int secret);
static void       _acquire_();
static void       _attr_(WINDOW* win, char ch, int on);
static void       _charout_(WINDOW* win, int row, int col, int ch, int pair);
static void       _clear_();
static CHAR       _code_sequence_(INT* erc, WINDOW* win, CHAR * filter);
static int        _color_(char *color);
static void       _curs_set_();
static void       _defaultField_(INT* erc, WINDOW* win, ctos_form* cfrm, ctos_field* cfld);
static int        _differs_(char *a, char *b);
static void       _edit_field_(INT* erc, WINDOW* win, ctos_form* cfrm, ctos_field* cfld, INITSTATE* initstate, EXITSTATE* exitstate);
static void       _end_screen_();
static char*      _format_(char* work, int len, const char *fmt, ...);
static CHAR       _get_code_(INT* erc, WINDOW* win);
static ctos_form* _getform_(INT *erc, CHAR* form);
static void       _getformfield_(INT *erc, ctos_form** cfrm, ctos_field** cfld, CHAR* form, CHAR* fieldname, INT fieldnamelen, INT index);
static WINDOW*    _getwin_(CHAR* form);
static int        _keyhandler_(WINDOW* win);
static void       _lineout_(WINDOW* win, ctos_form* cfrm);
static void       _load_fields_(INT* erc);
static void       _log_(char* line);
static void       _lpad_(CHAR* data, INT datalen);
static void       _ltrim_(CHAR* data, INT datalen);
static void       _release_();
static void       _rpad_(CHAR* data, INT datalen);
static void       _rtrim_(CHAR* data, INT datalen);
static void       _setwin_(CHAR* form, WINDOW* win);
static void       _show_clock_(void* arg);
static void       _show_time_(WINDOW* win);
static void       _show_title_(WINDOW* win);
static void       _show_header_(WINDOW* win, char* title);
static void       _set_sysid_(WINDOW* win, char* sysid);
static void       _show_sysid_(WINDOW* win);
static int        _showerror_(int error_rc, const char *fmt, ...);
static void       _start_screen_();
static void       _write_field_(INT* erc, WINDOW* win, ctos_form* cfrm, ctos_field* cfld, CHAR* data, INT datalen, int selected);

#endif
