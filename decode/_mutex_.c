#ifndef _mutex_c_
#define _mutex_c_

#ifdef DO_THREADING_ON
static void _acquire_()
{
  if (_semaphore_ == 0)
    _semaphore_ = semget(IPC_PRIVATE, 1, 0666 | IPC_CREAT);
  static struct sembuf acquireOp[] =  // The possible semaphore
  { 0, 0, 0                           // Wait for semaphore == 0
  , 0, 1, SEM_UNDO                    // inc semaphore if crash undo
  };
  semop(_semaphore_, acquireOp, 2);
}

static void _release_()
{
  static struct sembuf releaseOp[] =
  { 0, -1, SEM_UNDO  // dec semaphore
  };
  semop(_semaphore_, releaseOp, 1);
}

#define ACQUIRE() _acquire_() //,_log_("A"),fprintf(_logfile_, "%d\n", __LINE__),fflush(_logfile_)
#define RELEASE() _release_() //,_log_("R"),fprintf(_logfile_, "%d\n", __LINE__),fflush(_logfile_)
#else

#define ACQUIRE()
#define RELEASE()

#endif

#endif
