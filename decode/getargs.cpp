#include <stdio.h>
#include <string.h>
#include <ctype.h>

#if defined(WIN32)
#include <process.h>
#endif

#include "getargs.h"

#define ERRMSG "Illegal argument <%c>.  Legal arguments are:\n\n"

static ARG *findarg(int c, ARG *arg_vect, int no_of);
static char *setarg(ARG *arg, char *data);

static int stoi(char **instr)
{
  register int num = 0;
  register char *str;
  int sign = 1;

  str = *instr;
  while (*str == ' ' || *str == '\t' || *str == '\n')
    str++;
  if (*str == '-')
  {
    sign = -1;
    str++;
  }
  if (*str == '0')
  {
    ++str;
    if (*str == 'x' || *str == 'X')
    {
      str++;
      while (('0' <= *str && *str <= '9')
      ||   ('a' <= *str && *str <= 'f')
      ||   ('A' <= *str && *str <= 'F'))
      {
        num *= 16;
        num += ('0' <= *str && *str <= '9')
           ? *str - '0'
           : toupper(*str) - 'A' + 10;
        str++;
      }
    }
    else
    {
      while ('0' <= *str && *str <= '7')
      {
        num *= 8;
        num += *str++ - '0';
      }
    }
  }
  else
  {
    while ('0' <= *str && *str <= '9')
    {
      num *= 10;
      num += *str++ - '0';
    }
  }
  *instr = str;
  return num * sign;
}

static char *setarg(ARG *arg, char *data)
{
  switch (arg->type)
  {
  case INTEGER:
    *arg->variable = stoi(&data);
    break;
  case BOOLEAN:
    if (*data == '-')
    {
      *arg->variable = 0;
      data++;
    }
    else *arg->variable = 1;
    break;
  case CHARACTER:
    *arg->variable = *data;
    data++;
    break;
  case STRING:
    *(char **) arg->variable = data;
    data += strlen(data);
    break;
  default:
    printf("getargs : INTERNAL ERROR: BAD ARGUMENT TYPE\n");
    break;
  }
  return data;
}

static ARG *findarg(int c, ARG *arg_vect, int no_of)
{
  for (;--no_of >= 0;  arg_vect++)
    if (arg_vect->arg == (char) c)
      return arg_vect;
  return 0;
}

void pr_usage(ARG *arg_vect, int no_of)
{
  for (;--no_of >= 0;  arg_vect++)
  {
    switch (arg_vect->type)
    {
    case INTEGER:
      printf("-%c<num> %-40s [%-5d]\n",
             arg_vect->arg, arg_vect->errmsg, *(arg_vect->variable));
      break;
    case BOOLEAN:
      printf("-%c      %-40s [%-5s]\n",
             arg_vect->arg, arg_vect->errmsg, *(arg_vect->variable) ? "TRUE" : "FALSE");
      break;
    case CHARACTER:
      printf("-%c<c>   %-40s [%-5c]\n",
             arg_vect->arg, arg_vect->errmsg, *(arg_vect->variable));
      break;
    case STRING:
      printf("-%c<str> %-40s [%s]\n",
             arg_vect->arg, arg_vect->errmsg, *(char **) arg_vect->variable);
      break;
    }
  }
}

int get_args(int argc, char *argv[], ARG *arg_vect, int no_of)
{
  register int new_argc;
  register char **new_argv, *data;
  register ARG *arg;
  bool passthru = false;

  new_argc = 1;
  for (new_argv = ++argv;  --argc > 0;  argv++)
  {
    if (**argv !=  '-' || passthru == true)
    {
      *new_argv++ = *argv;
      new_argc++;
    }
    else
    {
      data = (*argv) + 1;
      while (*data)
      {
        if (*data == '-')
          passthru = true;
        else if ((arg = findarg(*data, arg_vect, no_of))!=0)
        {
          data++;
          if (arg->type == STRING && data[0] == 0 && argc > 0 && argv[1][0] != '-')
          {
            --argc;
            ++argv;
            data = *argv;
          }
          data = setarg(arg, data);
        }
        else
        {
          pr_usage(arg_vect, no_of);
          return -1;
        }
      }
    }
  }
  return new_argc;
}
