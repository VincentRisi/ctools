#ifndef _globals_c_
#define _globals_c_

typedef struct _Field_
{
  unsigned char code;
  unsigned char filter[MAX_FILTER_SIZE];
} _Field_;

typedef struct _Filter_
{
  unsigned short key;
  unsigned char filter[40];
} _Filter_;

const char *_errors_[] =
{ "All Cool"  
, "File path/name too long" 
, "Not enough file handles" 
, "File Open Error"
, "File handle out of range"
, "File already closed"
, "File not in use"
, "Not a valid form"
, "Form is not open"
, "Frame no out of range"
, "Field not in form"
, "B20CODES environment variable missing"
, "B20CODES file cannot be opened"
, "B20CODES file invalid signature"
, "B20CODES file invalid data"
, "WRITEALL INVALID DATA"
, "READALL INVALID DATA"
, "B20RECORD file cannot be opened"
, "B20SUBMIT file cannot be opened"
, "B20SUBMIT file invalid data"
};

enum
{ ERC_OK
, ERC_FILE_NAME_TOO_LONG
, ERC_NO_FILE_HANDLES
, ERC_FILE_OPEN_ERROR
, ERC_FILE_HANDLE_OUT_OF_RANGE
, ERC_FILE_ALREADY_CLOSED
, ERC_NO_FILE_IN_USE
, ERC_NOT_A_VALID_FORM
, ERC_FORM_IS_NOT_OPEN
, ERC_FRAME_IS_OUT_OF_RANGE
, ERC_FIELD_NOT_IN_FORM
, ERC_B20CODES_ENVVAR_MISSING
, ERC_B20CODES_FILE_CANNOT_BE_OPENED
, ERC_B20CODES_INVALID_SIGNATURE
, ERC_B20CODES_INVALID_DATA
, ERC_WRITEALL_INVALID_DATA
, ERC_READALL_INVALID_DATA
, ERC_B20RECORD_FILE_CANNOT_BE_OPENED
, ERC_B20SUBMIT_FILE_CANNOT_BE_OPENED
, ERC_B20SUBMIT_INVALID_DATA
};

static FILE*           _fh_[MAX_FILES];
static FILE*           _recorder_;
static _Field_         _field_[MAX_NO_FIELDS];
static int             _files_used_[MAX_FILES];
static int             _no_fields_;
static int             _no_files_;
static int             _insert_mode_;
static int             _already_started_;
static int             _started_;
static int             _semaphore_;
static int             _submit_chars_len_;
static int             _submit_chars_index_;
static int             _screensaved;
static WINDOW*         _mainwin_;
static WINDOW*         _topwin_;
static pthread_t       _thread_;
static b20_thread_data _thread_data_;
static pthread_attr_t  _thread_attr_;
static unsigned char*  _submit_chars_;

#endif
