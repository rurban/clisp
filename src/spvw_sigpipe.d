# Handling of signal SIGPIPE.

# ------------------------------ Specification ---------------------------------

#if defined(HAVE_SIGNALS) && defined(SIGPIPE)

# Variable to be set ONLY during write() calls to pipes directed to
# subprocesses.
  global boolean writing_to_subprocess = FALSE;

# Install the signal handler for SIGPIPE.
  local void install_sigpipe_handler (void);

#endif

# ------------------------------ Implementation --------------------------------

#if defined(HAVE_SIGNALS) && defined(SIGPIPE)

# SIGPIPE is the signal sent to a process calling write() on a pipe with no
# readers. Together with the signal, the write() call is terminated with
# return value -1, errno = EPIPE. Note that SIGPIPE is a *synchronous* signal:
# it occurs only during write(), without delay (unless blocked).

# The default action of SIGPIPE signals is to terminate the process without
# core dump. This is fine for pipes that did exist prior to ourselves, but is
# not what we want when we write to subprocesses we have spawned using
# MAKE-PIPE-OUTPUT-STREAM or MAKE-PIPE-IO-STREAM. In this case we want a normal
# error message, and not die of the signal.

# Signal handler for SIGPIPE:
  local void sigpipe_handler (int sig);
  local void sigpipe_handler(sig)
    var int sig; # sig = SIGPIPE
    { signal_acknowledge(SIGPIPE,&sigpipe_handler);
      if (writing_to_subprocess)
        # Ignore the signal, the write() error code is sufficient.
        return;
      # Revert to the default handler and re-raise the signal.
      # This should be sufficient to kill us.
      SIGNAL(SIGPIPE,SIG_DFL);
      #if !(defined(UNIX) && !defined(HAVE_RAISE))
      raise(SIGPIPE);
      #else
      kill(getpid(),SIGPIPE);
      #endif
    }

#define install_sigpipe_handler()  \
  SIGNAL(SIGPIPE,&sigpipe_handler);

#endif

