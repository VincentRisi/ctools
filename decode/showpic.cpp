#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sox.h"

static int show_error(SockCB &sox, int rc, const char* from)
{
  int eno;
  printf("FAILED: %s, %d:%s (%d) %s\n", from, rc, soxReturnCode(rc), (eno = soxError(sox)), soxErrorText(eno));
  return rc;
}

static int display_pic(const char* picture, const char* host, const char* port)
{
  SockCB sox;
  char buffer[512];
  unsigned int read;
  int rc = 0, eno;
  if (rc = soxClientInit(sox, host, port))
    return show_error(sox, rc, "ClientInit");
  if (rc = soxSetTimeOut(sox, 10000))
    return show_error(sox, rc, "SetTimeout");
  if (rc = soxClientOpen(sox))
    return show_error(sox, rc, "ClientOpen");
  if (rc = soxWrite(sox, (void*)picture, strlen(picture)+1))
    return show_error(sox, rc, "Write"); 
  if (rc = soxStreamRead(sox, buffer, sizeof(buffer), read))
    return show_error(sox, rc, "StreamRead"); 
  if (rc = soxClose(sox))
    return show_error(sox, rc, "Close"); 
  if (rc = soxDone(sox))
    return show_error(sox, rc, "Done"); 
  printf("SUCCEEDED: %s\n", buffer);
  return 0;
}

int main(int argc, char* argv[])
{
  enum {e_program, e_picture, e_port, e_host};
  char* program = argv[e_program];
  char *host = "";
  if (argc <= e_host)
  {
    char* picture_server = getenv("PICTURE_SERVER");
    if (picture_server != 0)
    {
      host = strdup(picture_server);
    }
    else
    {
      char* ssh_client = getenv("SSH_CLIENT"); 
      if (ssh_client != 0)
      {
        host = strdup(ssh_client);
        char* x = strchr(host, ' ');
        if (x) *x = 0;
      }
    }
  }
  char* port = "32145";
  char* picture;
  if (argc <= e_picture)
  {
    printf("usage:\n"
           "%s <picture> <port:32145> <host:%s>\n", program, host);
    return 1;
  }
  if (argc > e_picture)
    picture = strdup(argv[e_picture]);
  if (argc > e_port)
    port = strdup(argv[e_port]);
  if (argc > e_host)
    host = strdup(argv[e_host]);
  printf("picture:%s port:%s host:%s\n", picture, port, host);
  return display_pic(picture, host, port);
}