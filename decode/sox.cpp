#include <stdlib.h>
#include <string.h>
#include "sox.h"

static int     _sockInit(SockCB &sockCB, const char *host, const char *service);
static int     _wait(int port, unsigned int mSec, bool onRead, bool &result);
static int     _readAll(SockCB &sockCB, char *buff, int size);
static void    _setError(SockCB &sockCB);
static void    _forceClose(SockCB &sockCB);

int soxDuplicate(SockCB &sockCB, int commsPort, int retries, int timeOut)
{
  memset(&sockCB, 0, sizeof(sockCB));
  sockCB.listenPort = -1;
  sockCB.commsPort  = commsPort;
  sockCB.retries    = (unsigned short)retries;
  sockCB.timeOut    = timeOut;
  return 0;
}

int soxDupServer(SockCB &sockCB, int listenPort, int retries, const int timeOut)
{
  memset(&sockCB, 0, sizeof(sockCB));
  sockCB.listenPort = listenPort;
  sockCB.commsPort  = -1;
  sockCB.retries    = (unsigned short)retries;
  sockCB.timeOut    = timeOut;
  return 0;
}

int soxServerInit(SockCB &sockCB, const char *service)
{
  int rc;
  rc = _sockInit(sockCB, 0, service);
  return rc;
}

int soxServerOpen(SockCB &sockCB)
{
  sockCB.addrLen   = sizeof(sockCB.addr);
  sockCB.commsPort = (unsigned int)accept(sockCB.listenPort, (sockaddr*)&sockCB.addr, &sockCB.addrLen);
  if (sockCB.commsPort == -1)
  {
    _setError(sockCB);
    return errSockSocket;
  }
#if defined(TCP_NODELAY)
  int on = 1;
  setsockopt(sockCB.commsPort, SOL_SOCKET, TCP_NODELAY,(char*)&on, sizeof(on));
#endif
  return 0;
}

int soxClientInit(SockCB &sockCB, const char *host, const char *service)
{
  int rc;
  rc = _sockInit(sockCB, host, service);
  return rc;
}

static void _forceClose(SockCB &sockCB)
{
  if (sockCB.commsPort != -1)
    soclose(sockCB.commsPort);
  sockCB.commsPort = -1;
}

int soxClientOpen(SockCB &sockCB)
{
  int rc;
  sockCB.commsPort = socket(AF_INET, SOCK_STREAM, 0);
  if (sockCB.commsPort == -1)
  {
    _setError(sockCB);
    return errSockSocket;
  }
  rc = connect(sockCB.commsPort, (sockaddr*)&sockCB.addr, sizeof(sockCB.addr));
  if (rc)
  {
    _setError(sockCB);
    _forceClose(sockCB);
    return errSockConnect;
  }
#if defined(TCP_NODELAY)
  int delay_on = 1;
  setsockopt(sockCB.commsPort, SOL_SOCKET, TCP_NODELAY,(char*)&delay_on, sizeof(delay_on));
#endif
#if defined(SO_USELOOPBACK)
  int loop_on = 1;
  setsockopt(sockCB.commsPort, SOL_SOCKET, SO_USELOOPBACK, (char*)&loop_on, sizeof(loop_on));
#endif
  return 0;
}

int soxSetTimeOut(SockCB &sockCB, unsigned int timeOut)
{
  sockCB.timeOut = timeOut;
  return 0;
}

int soxSetRetries(SockCB &sockCB, unsigned short retries)
{
  sockCB.retries = retries;
  return 0;
}

int soxDone(SockCB &sockCB)
{
  if (sockCB.commsPort != -1)
    soclose(sockCB.commsPort);
  if (sockCB.listenPort != -1)
    soclose(sockCB.listenPort);
  sockCB.commsPort  = -1;
  sockCB.listenPort = -1;
  return 0;
}

int soxClose(SockCB &sockCB)
{
#if defined(SO_LINGER)
  struct linger x = {1, 5};
  setsockopt(sockCB.commsPort, SOL_SOCKET, SO_LINGER,(char*)&x, sizeof(x));
#endif
  if (sockCB.commsPort != -1)
  {
#if defined(SD_BOTH)
    shutdown(sockCB.commsPort, SD_BOTH); // Vince adding this to see if WAITS reduce faster
#elif defined(SHUT_RDWR)
    shutdown(sockCB.commsPort, SHUT_RDWR); // Vince adding this to see if WAITS reduce faster
#endif
    soclose(sockCB.commsPort);
  }
  sockCB.commsPort = -1;
  return 0;
}

int soxReadLength(SockCB &sockCB, unsigned int &size)
{
  unsigned int readSize;
  int n = _readAll(sockCB, (char *)&readSize, sizeof(readSize));
  if (n != sizeof(unsigned int))
    return errSockRead;
  size = ntohl(readSize);
  return 0;
}

int soxRead(SockCB &sockCB, void *buffer, unsigned int bufferLen)
{
  int n = _readAll(sockCB, (char *)buffer, bufferLen);
  if ((unsigned int)n != bufferLen)
    return errSockRead;
  return 0;
}

int soxStreamRead(SockCB &sockCB, void *buffer, unsigned int bufferLen, unsigned int &size)
{
  int n = recv(sockCB.commsPort, (char *) buffer, bufferLen, 0);
  if (n < 0)
    return errSockRead;
  size = n;
  return 0;
}

int soxWaitRead(SockCB &sockCB, unsigned int timeOut, bool &result)
{
  return _wait(sockCB.commsPort, timeOut, true, result);
}

int soxWaitWrite(SockCB &sockCB, unsigned int timeOut, bool &result)
{
  return _wait(sockCB.commsPort, timeOut, false, result);
}

int soxWaitServer(SockCB &sockCB, unsigned int timeOut, bool &result)
{
  return _wait(sockCB.listenPort, timeOut, true, result);
}

int soxWriteLength(SockCB &sockCB, unsigned int length)
{
  unsigned int writeLength = htonl(length);
  int n;
  n = send(sockCB.commsPort, (char *)&writeLength, sizeof(writeLength), 0);
  if (n != sizeof(writeLength))
  {
    _setError(sockCB);
    return errSockWrite;
  }
  return 0;
}

int soxWrite(SockCB &sockCB, void *buffer, unsigned int bufferLen)
{
  int n;
  n = send(sockCB.commsPort, (char *)buffer, bufferLen, 0);
  if ((unsigned int)n != bufferLen)
  {
    _setError(sockCB);
    return errSockWrite;
  }
  return 0;
}

int soxError(SockCB &sockCB)
{
  return sockCB.errorNo;
}

const char *soxErrorMsg(SockCB &sockCB)
{
  return soxErrorText(sockCB.errorNo);
}

int soxErrorMsgNet(SockCB &sockCB, char *buffer, int bufferLen)
{
  int i;
  strncpy(buffer, soxErrorMsg(sockCB), bufferLen-1);
  buffer[bufferLen-1] = 0;
  for (i = strlen(buffer); i < bufferLen; i++)
    buffer[i] = ' ';
  return sockCB.errorNo;
}

static int _sockInit(SockCB &sockCB, const char *host, const char *service)
{
  int             rc;
  servent         *ps;
  hostent         *ph;
  sockaddr_in      addr;
  int listenPort = -1;
  int isNo, i;
  memset(&sockCB, 0, sizeof(sockCB));
#if defined(__MSVC__) || defined(__WIN32__) || _MSC_VER >= 1200
  WSADATA  wsadata;
  rc = WSAStartup(DESIRED_WINSOCK_VERSION, &wsadata);
  if (rc)
  {
    rc = sockCB.errorNo = WSAGetLastError();
    return rc;
  }
  if (wsadata.wVersion < MINIMUM_WINSOCK_VERSION)
  {
    rc = errSockVersion;
    _setError(sockCB);
    return rc;
  }
#endif
  rc = 0;
  for (isNo = 1, i = 0; service[i]; i++)
  {
    if (service[i] < '0' || service[i] > '9')
    {
      isNo = 0;
      break;
    }
  }
  if (isNo == 1)
    addr.sin_port = htons((short)atoi(service));
  else
  {
    ps = (servent*)getservbyname(service, PROTOCOL);
    if (ps == 0)
    {
      rc = errSockService;
      _setError(sockCB);
      return rc;
    }
    addr.sin_port = ps->s_port;
  }
  if (host != 0)
  {
    ph = (hostent*)gethostbyname(host);
    if (ph == 0)
    {
      rc = errSockHost;
      _setError(sockCB);
      return rc;
    }
    addr.sin_family = ph->h_addrtype;
    memcpy(&addr.sin_addr, ph->h_addr, ph->h_length);
  }
  else
  {
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_family = AF_INET;
    listenPort = socket(AF_INET, SOCK_STREAM, 0);
    if (listenPort == -1)
    {
      rc = errSockSocket;
      _setError(sockCB);
      return rc;
    }
    int on=1;
#if defined(SO_REUSEADDR)
    if (setsockopt(listenPort,SOL_SOCKET,SO_REUSEADDR,(char*)&on,
      sizeof(on))<0)
    {
      rc = errSockSocket;
      _setError(sockCB);
      return rc;
    }
#endif
#if defined(NO_SO_REUSEPORT)
    if (setsockopt(listenPort,SOL_SOCKET,SO_REUSEPORT,(char*)&on,
      sizeof(on))<0)
    {
      rc = errSockSocket;
      _setError(sockCB);
      return rc;
    }
#endif
    rc=bind(listenPort, (struct sockaddr*)&addr, sizeof(addr));
    if (rc == -1)
    {
      rc = errSockBind;
      _setError(sockCB);
      return rc;
    }
    rc=listen(listenPort, 5);
    if (rc == -1)
    {
      rc = errSockListen;
      _setError(sockCB);
      return rc;
    }
    if (addr.sin_port == 0)
    {
#if defined(__GNUC__)
      socklen_t len = sizeof(addr);
#else      
      int len = sizeof(addr);
#endif      
      getsockname(listenPort, (struct sockaddr*)&addr, &len);
    }
  }
  sockCB.listenPort = listenPort;
  sockCB.commsPort = -1;
  sockCB.retries = 5;
  sockCB.timeOut = 10000;
  sockCB.addr = addr;
  if (host != 0)
    sockCB.addr.sin_addr.s_addr = *((unsigned int *)ph->h_addr);
  else
    sockCB.addr.sin_addr.s_addr = INADDR_ANY;
  sockCB.addr.sin_family = AF_INET;
  return rc;
}

static int _wait(int port, unsigned int mSec, bool onRead, bool &result)
{
  struct timeval timeOut;
  struct timeval *top;
  if (mSec)
  {
    timeOut.tv_sec  = mSec / 1000;
    timeOut.tv_usec = (mSec % 1000) * 1000;
    top = &timeOut;
  }
  else
    top = 0;
  fd_set onSet;
  FD_ZERO(&onSet);
  FD_SET((unsigned)port, &onSet);
  int rc = select(FD_SETSIZE, (onRead == true?&onSet:0), (onRead==true?0:&onSet), 0, top);
  if (rc <= 0)
    return errSockTimeout;
  result = FD_ISSET(port, &onSet) ? true : false;
  return 0;
}

static int _readAll(SockCB &sockCB, char *buff, int size)
{
  int bytesRead = 0, n, tryNo = 0;
  bool result;

  while (bytesRead < size)
  {
    if (sockCB.timeOut)
    {
      if (_wait(sockCB.commsPort, sockCB.timeOut, true, result)==0)
      {
        if (result == false)
        {
          if (tryNo++ < sockCB.retries)
            continue;
          else
            return -1;
        }
      }
      else
      {
        return -1;
      }
    }
    n = recv(sockCB.commsPort, buff, size - bytesRead, 0);
    if (n < 0) // underflow ??
      return n;
    if (n == 0)
      return bytesRead;
    bytesRead += n;
    buff += n;
  }
  return bytesRead;
}

static void _setError(SockCB &sockCB)
{
#if defined(__GNUC__)
  sockCB.errorNo = errno;
#else
  sockCB.errorNo = WSAGetLastError();
#endif
}

const char *soxReturnCode(int rc)
{
  switch (rc)
  { 
    case errSockInit: return "Sock Init Error";
    case errSockHost: return "Sock Host Error";
    case errSockService: return "Sock Service Error";
    case errSockSocket: return "Sock Socket Error";
    case errSockBind: return "Sock Bind Error";
    case errSockListen: return "Sock Listen Error";
    case errSockConnect: return "Sock Connect Error";
    case errSockClose: return "Sock Close Error";
    case errSockRead: return "Sock Read Error";
    case errSockWrite: return "Sock Write Error";
    case errSockVersion: return "Sock Version Error";
    case errSockIOCtl: return "Sock IOCtl Error";
    case errSockLinger: return "Sock Linger Error";
    case errSockKeepAlive: return "Sock KeepAlive Error";
    case errSockTimeout: return "Sock Timeout Error";
    case errSockDebug: return "Sock Debug Error";
    case errSockGetOpt: return "Sock GetOpt Error";
    case errSockSetOpt: return "Sock SetOpt Error";
  }
  return "unknown";
}

const char *soxErrorText(int rc)
{
  switch (rc)
  {
#if defined(__GNUC__)
  case EPERM:            return "Operation not permitted";
  case ENOENT:           return "No such file or directory";
  case ESRCH:            return "No such process";
  case EINTR:            return "interrupted system call";
  case EIO:              return "I/O error";
  case ENXIO:            return "No such device or address";
  case E2BIG:            return "Arg list too long";
  case ENOEXEC:          return "Exec format error";
  case EBADF:            return "Bad file descriptor";
  case ECHILD:           return "No child processes";
  case EAGAIN:           return "Resource temporarily unavailable";
  case ENOMEM:           return "Not enough space";
  case EACCES:           return "Permission denied";
  case EFAULT:           return "Bad address";
  case ENOTBLK:          return "Block device required";
  case EBUSY:            return "Resource busy";
  case EEXIST:           return "File exists";
  case EXDEV:            return "Improper link";
  case ENODEV:           return "No such device";
  case ENOTDIR:          return "Not a directory";
  case EISDIR:           return "Is a directory";
  case EINVAL:           return "Invalid argument";
  case ENFILE:           return "Too many open files in system";
  case EMFILE:           return "Too many open files";
  case ENOTTY:           return "Inappropriate I/O control operation";
  case ETXTBSY:          return "Text file busy";
  case EFBIG:            return "File too large";
  case ENOSPC:           return "No space left on device";
  case ESPIPE:           return "Invalid seek";
  case EROFS:            return "Read only file system";
  case EMLINK:           return "Too many links";
  case EPIPE:            return "Broken pipe";
  case EDOM:             return "Domain error within math function";
  case ERANGE:           return "Result too large";
  case ENOMSG:           return "No message of desired type";
  case EIDRM:            return "Identifier removed";
  case ECHRNG:           return "Channel number out of range";
  case EL2NSYNC:         return "Level 2 not synchronized";
  case EL3HLT:           return "Level 3 halted";
  case EL3RST:           return "Level 3 reset";
  case ELNRNG:           return "Link number out of range";
  case EUNATCH:          return "Protocol driver not attached";
  case ENOCSI:           return "No CSI structure available";
  case EL2HLT:           return "Level 2 halted";
  case EDEADLK:          return "Resource deadlock avoided";
  case ENOLCK:           return "No locks available";
  case ESTALE:           return "no filesystem";
  case EINPROGRESS:      return "Operation now in progress";
  case EALREADY:         return "Operation already in progress";
  case ENOTSOCK:         return "Socket operation on non-socket";
  case EDESTADDRREQ:     return "Destination address required";
  case EMSGSIZE:         return "Message too long";
  case EPROTOTYPE:       return "Protocol wrong type for socket";
  case ENOPROTOOPT:      return "Protocol not available";
  case EPROTONOSUPPORT:  return "Protocol not supported";
  case ESOCKTNOSUPPORT:  return "Socket type not supported";
  case EOPNOTSUPP:       return "Operation not supported on socket";
  case EPFNOSUPPORT:     return "Protocol family not supported";
  case EAFNOSUPPORT:     return "Address family not supported by protocol family";
  case EADDRINUSE:       return "Address already in use";
  case EADDRNOTAVAIL:    return "Can't assign requested address";
  case ENETDOWN:         return "Network is down";
  case ENETUNREACH:      return "Network is unreachable";
  case ENETRESET:        return "Network dropped connection on reset";
  case ECONNABORTED:     return "Software caused connection abort";
  case ECONNRESET:       return "Connection reset by peer";
  case ENOBUFS:          return "No buffer space available";
  case EISCONN:          return "Socket is already connected";
  case ENOTCONN:         return "Socket is not connected";
  case ESHUTDOWN:        return "Can't send after socket shutdown";
  case ETIMEDOUT:        return "Connection timed out";
  case ECONNREFUSED:     return "Connection refused";
  case EHOSTDOWN:        return "Host is down";
  case EHOSTUNREACH:     return "No route to host";
  case ERESTART:         return "restart the system call";
  case EUSERS:           return "Too many users";
  case ELOOP:            return "Too many levels of symbolic links";
  case ENAMETOOLONG:     return "File name too long";
  case EREMOTE:          return "Item is not local to host";
  case ENOSYS:           return "Function not implemented  POSIX";
  case ETOOMANYREFS:     return "Too many references: can't splice";
  case EILSEQ:           return "Invalid wide character";
  case ECANCELED:        return "asynchronous i/o cancelled";
  case ENOSR:            return "temp out of streams resources";
  case ETIME:            return "I_STR ioctl timed out";
  case EBADMSG:          return "wrong message type at stream head";
  case EPROTO:           return "STREAMS protocol error";
  case ENODATA:          return "no message ready at stream head";
  case ENOSTR:           return "fd is not a stream";
  case EMULTIHOP:        return "multihop is not allowed";
  case ENOLINK:          return "the link has been severed";
  case EOVERFLOW:        return "value too large to be stored in data type";
#else
  case WSAEACCES:           return "Access denied";
  case WSAEADDRINUSE:       return "Address already in use";
  case WSAEADDRNOTAVAIL:    return "Can't assign requested address";
  case WSAEAFNOSUPPORT:     return "Address family not supported by protocol family";
  case WSAEALREADY:         return "Operation already in progress";
  case WSAEBADF:            return "Bad file number";
  case WSAECONNABORTED:     return "Software caused connection abort";
  case WSAECONNREFUSED:     return "Connection refused";
  case WSAECONNRESET:       return "Connection reset by peer";
  case WSAEDESTADDRREQ:     return "Destination address required";
  case WSAEDISCON:          return "Disconnected";
  case WSAEDQUOT:           return "Disk quota exceeded";
  case WSAEFAULT:           return "Bad address";
  case WSAEHOSTDOWN:        return "Host is down";
  case WSAEHOSTUNREACH:     return "Host is unreachable";
  case WSAEINPROGRESS:      return "Operation now in progress";
  case WSAEINTR:            return "Interrupted system call";
  case WSAEINVAL:           return "Invalid argument";
  case WSAEISCONN:          return "Socket is already connected";
  case WSAELOOP:            return "Too many levels of symbolic links";
  case WSAEMFILE:           return "Too many open files";
  case WSAEMSGSIZE:         return "Message too long";
  case WSAENAMETOOLONG:     return "Name too long";
  case WSAENETDOWN:         return "Network is down";
  case WSAENETRESET:        return "Network was reset";
  case WSAENETUNREACH:      return "ICMP network unreachable";
  case WSAENOBUFS:          return "No buffer space is supported";
  case WSAENOPROTOOPT:      return "Bad protocol option";
  case WSAENOTCONN:         return "Socket is not connected";
  case WSAENOTEMPTY:        return "Directory not empty";
  case WSAENOTSOCK:         return "Socket operation on non-socket";
  case WSAEOPNOTSUPP:       return "Operation not supported on socket";
  case WSAEPFNOSUPPORT:     return "Protocol family not supported";
  case WSAEPROCLIM:         return "EPROCLIM returned";
  case WSAEPROTONOSUPPORT:  return "Protocol not supported";
  case WSAEPROTOTYPE:       return "Protocol is wrong type for socket";
  case WSAEREMOTE:          return "The object is remote";
  case WSAESHUTDOWN:        return "Can't send after socket shutdown";
  case WSAESOCKTNOSUPPORT:  return "Socket type not supported";
  case WSAESTALE:           return "ESTALE returned";
  case WSAETIMEDOUT:        return "Connection timed out";
  case WSAETOOMANYREFS:     return "Too many references";
  case WSAEUSERS:           return "EUSERS returned";
  case WSAEWOULDBLOCK:      return "Operation would block";
  case WSAHOST_NOT_FOUND:   return "Host not found";
  case WSANOTINITIALISED:   return "Not initialized";
  case WSANO_DATA:          return "No data record available";
  case WSANO_RECOVERY:      return "Non-recoverable error";
  case WSASYSNOTREADY:      return "System not ready";
  case WSATRY_AGAIN:        return "Try again";
  case WSAVERNOTSUPPORTED:  return "Version is not supported";
#endif
  }
  return "Unknown Error";
}
