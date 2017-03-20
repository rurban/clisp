/*
 * Setting up a connection to an X server, and other socket functions
 * Bruno Haible 19.6.1994, 27.6.1997, 9.3.1999 ... 2003
 * Marcus Daniels 28.9.1995, 9.9.1997
 * Sam Steingold 1998-2012
 * German comments translated into English: Stefan Kain 2002-09-11
 */

/* The code in function connect_to_x_server originally came from the X11R5
 distribution, file mit/X/XConnDis.c, with the following modifications:
 - no support for DNETCONN or STREAMSCONN,
 - display name has already been split into hostname and display number,
 - doesn't return full host&display name and auth info,
 - doesn't depend on the X include files,
 - support IPv6. */

/* mit/X/XConnDis.c carries the following copyright notice: */
/*
 * $XConsortium: XConnDis.c,v 11.85 91/07/19 23:07:39 gildea Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *
 * This file contains operating system dependencies.
 */

#include "lispbibl.c"

#if defined(UNIX) || defined(WIN32_NATIVE)

/* ============ hostnames and IP addresses only (no sockets) ============

 Fetches the machine's host name.
 get_hostname(hostname);
 where hostname is an array of MAXHOTNAMELEN+1 characters.
 < const char host[]: The host name.
 (Note: In some cases we could get away with less system calls by simply
 setting
   host = "localhost";
 But this seems not worth the trouble to think about it.)
 sds: never: you will always get localhost/127.0.0.1 - what's the point? */
#if defined(HAVE_GETHOSTNAME)
/* present on all supported unix systems and on woe32 */
static void get_hostname (char *hostname) {
  begin_system_call();
  if (gethostname(hostname,MAXHOSTNAMELEN) < 0) { ANSIC_error(); }
  end_system_call();
  hostname[MAXHOSTNAMELEN] = '\0';
}
#else
  #error get_hostname is not defined
#endif

#if defined(TCPCONN)
  #include <netinet/in.h> /* declares htons(), defines struct sockaddr_in */
  #include <arpa/inet.h> /* declares inet_pton(), inet_ntop() */
  #ifdef IPV6_NEED_LINUX_IN6_H
    #include <linux/in6.h> /* defines struct in6_addr, struct sockaddr_in6 */
  #endif
  #if defined(HAVE_IPV6) && defined(UNIX_DARWIN)
    /* Access the internals of a 'struct in6_addr'. */
    #define in6_u __u6_addr
    #define u6_addr16 __u6_addr16
  #endif
  #if defined(HAVE_IPV6) && defined(WIN32)
    #define in6_addr in_addr6
  #endif
#endif

/* "Older BSDs" (specifically, Mac OS X 10.4.[34]) require that
   sockaddr is filled with 0 before use (i.e., unused fields must be 0)
   and this requirement _IS_ codified in
   + the 1st ed of Stevens "UNIX Network Programming"
   + "Networking and the BSD Sockets API"
     http://www.macdevcenter.com/pub/a/mac/2002/12/26/cocoa.html?page=3
   + tutorial "Socket programming"
     http://micmacfr.homeunix.org/progsysdet/progsys12.shtml
   + http://lists.apple.com/archives/Unix-porting/2006/Feb/msg00053.html
     claims that "RFC 793, page 31, figure 8" means that
     the "unused fields" are not really unused
     see http://rfc.net/rfc793.html (figure 8 is actually on page 32)
   but _NOT_ in
   - the 3rd ed of Stevens "UNIX Network Programming"
   - the 2nd ed of Stevens "Advanced Programming in the UNIX Environment"
   - the 2nd ed of Rochkind "Advanced UNIX Programming"
   - the Open Group Base Specifications Issue 6 IEEE Std 1003.1, 2004 Edition
     http://www.opengroup.org/onlinepubs/009695399/
   It is _NOT_ necessary on Linux, Cygwin, Win32, Solaris, NetBSD, FreeBSD &c.
   It might be a good idea to add an autoconf test for this ...
   see https://sourceforge.net/p/clisp/bugs/304/ */
#define FILL0(s)  memset((void*)&s,0,sizeof(s))

/* Convert the IP address from C format to Lisp
 > type: address type (AF_INET..)
 > addr: whatever the address is for this type
 < lisp string representing the address in a human-readable format
 for syscalls & rawsock modules
 can trigger GC */
modexp maygc object addr_to_string (short type, char *addr) {
  var char buffer[MAXHOSTNAMELEN];
  return safe_to_string(inet_ntop(type,addr,buffer,MAXHOSTNAMELEN));
}

LISPFUNN(machine_instance,0)
{ /* (MACHINE-INSTANCE), CLTL p. 447 */
  var object result = O(machine_instance_string);
  if (nullp(result)) { /* still unknown? */
    /* yes -> query hostname and fetch its internet address:
       (let* ((hostname (unix:gethostname))
              (address (unix:gethostbyname hostname)))
         (if (or (null address) (zerop (length address)))
           hostname
           (apply #'string-concat hostname " [" (inet-ntop address) "]"))) */
    var char host[MAXHOSTNAMELEN+1];
    get_hostname(host);
    result = asciz_to_string(host,O(misc_encoding)); /* hostname as result */
   #ifdef HAVE_GETHOSTBYNAME
    pushSTACK(result); /* hostname as 1st string */
    {
      var uintC stringcount = 1;
      /* fetch internet information: */
      var struct hostent * h;
      begin_system_call();
      h = gethostbyname(host);
      end_system_call();
      if (h) {
        STACK_0 = asciz_to_string(h->h_name,O(misc_encoding));
        if (h->h_addr && (h->h_length > 0)) {
          object hostname = addr_to_string(h->h_addrtype,h->h_addr);
          if (!nullp(hostname)) {
              pushSTACK(NIL); pushSTACK(hostname);
              STACK_1 = ascii_to_string(" [");
              pushSTACK(ascii_to_string("]"));
              stringcount += 3;
          }
        }
      }
      /* concatenate Strings: */
      result = string_concat(stringcount);
    }
   #endif
    /* we store the result for the next time: */
    O(machine_instance_string) = result;
  }
  VALUES1(result);
}

/* We assume that if we have gethostbyname(), we have a networking OS
   (Unix or Win32). */
#ifdef HAVE_GETHOSTBYNAME

/* ====================== General socket utilities ====================== */

#if defined(UNIXCONN) || defined(TCPCONN)

#ifndef CLOSE               /* win32 */
/* A wrapper around the close() function. */
#define CLOSE(fd)  \
  do { while ((close(fd) < 0) && errno == EINTR) ; } while (0)
#endif

/* A wrapper around the connect() function.
 To be used inside begin/end_system_call() only. */
global int nonintr_connect (SOCKET fd, struct sockaddr * name, int namelen) {
  var int retval;
  do {
    retval = connect(fd,name,namelen);
  } while ((retval < 0) && errno == EINTR);
  return retval;
}
#undef connect  /* because of UNIX_CYGWIN32 */
#define connect nonintr_connect

/* Execute a statement, but save errno during it.
 NB: gnulib ensures that the win32 errors are in errno, not WSA */
#define saving_errno(statement)                                         \
    do { int _olderrno = errno; statement; errno = _olderrno; } while(0)

#endif /* UNIXCONN || TCPCONN */

#if defined(TCPCONN)

/* for syscalls and rawsock modules */
typedef int (*host_fn_t) (const void* addr, int addrlen, int family, void* opts);
/* call FN on host/size/opts if HOST is an IP address */
local int with_host (const char* host, host_fn_t fn, void* opts) {
 #ifdef HAVE_IPV6
  {
    var struct in6_addr inaddr;
    if (inet_pton(AF_INET6,host,&inaddr) > 0)
      return fn(&inaddr,sizeof(struct in6_addr),AF_INET6,opts);
  }
 #endif
  {
    var struct in_addr inaddr;
    if (inet_pton(AF_INET,host,&inaddr) > 0)
      return fn(&inaddr,sizeof(struct in_addr),AF_INET,opts);
  }
  return fn(host,0,0,opts);
}

local int string_to_addr1 (const void* addr, int addrlen, int family, void* ret)
{
  *(object*)ret = (addrlen
                   ? data_to_sb8vector(addr,addrlen)
                   : asciz_to_string((const char*)addr,O(misc_encoding)));
  (void)family; /* ignore */
  return 0;
}

/* Convert the IP address from C format to Lisp
 > name: FQDN or dotted quad or IPv6 address
 < lisp string for FQDN or byte vector for IPv[46] numerics
 for syscalls & rawsock modules
 can trigger GC */
modexp maygc object string_to_addr (const char* name) {
  object ret;
  begin_system_call();
  with_host(name,&string_to_addr1,&ret);
  end_system_call();
  return ret;
}

local int resolve_host1 (const void* addr, int addrlen, int family, void* ret) {
  *(struct hostent**)ret =
    (addrlen
     ? gethostbyaddr((const char*)addr,addrlen,family)
     : gethostbyname((const char*)addr));
  return 0;
}

/* Return the hostent specified by the host designator
 > arg: host name designator:
        :DEFAULT - current host
        string/symbol: FQDN is resolved (gethostbyname)
        uint32: raw IPv4 address (gethostbyaddr)
        uint128: raw IPv6 address (gethostbyaddr)
        bit vector: raw IPv4[46] address (gethostbyaddr)
 < static hostent descriptor from LIBC
 for syscalls & rawsock modules */
modexp struct hostent* resolve_host (object arg) {
  var struct hostent* he;
  if (eq(arg,S(Kdefault))) {
    var char host[MAXHOSTNAMELEN+1];
    get_hostname(host);
    begin_system_call();
    he = gethostbyname(host);
    end_system_call();
  } else if (stringp(arg) || symbolp(arg)) {
    with_string_0(stringp(arg)?arg:(object)Symbol_name(arg),
                  O(misc_encoding), namez, {
      begin_system_call();
      with_host(namez,&resolve_host1,&he);
      end_system_call();
    });
  } else if (uint32_p(arg)) {
    var struct in_addr addr;
    UI_to_LEbytes(arg,8*sizeof(struct in_addr),(uintB*)&addr);
    begin_system_call();
    he = gethostbyaddr((char*)&addr,sizeof(struct in_addr),AF_INET);
    end_system_call();
  } else if (vectorp(arg)) {
    /* bit vector: treat as raw IP address data */
    var uintL vec_len = vector_length(arg);
    var uintL data_size;            /* size of data in byte */
    switch (array_atype(arg)) {
      /* vec_len must be a whole number of bytes */
      case Atype_Bit:   data_size = vec_len>>3;
        if (vec_len & 7) goto resolve_host_bad_vector;
        break;
      case Atype_2Bit:  data_size = vec_len>>2;
        if (vec_len & 3) goto resolve_host_bad_vector;
        break;
      case Atype_4Bit:  data_size = vec_len>>1;
        if (vec_len & 1) goto resolve_host_bad_vector;
        break;
      case Atype_8Bit:  data_size = vec_len;    break;
      case Atype_16Bit: data_size = vec_len<<1; break;
      case Atype_32Bit: data_size = vec_len<<2; break;
      default: goto resolve_host_bad_vector;
    }
    if (data_size == sizeof(struct in_addr)) {
      var uintL index = 0;
      var object data = array_displace_check(arg,vec_len,&index);
      begin_system_call();
      he = gethostbyaddr((char*)(TheSbvector(data)->data+index),
                         data_size,AF_INET);
      end_system_call();
    }
   #ifdef HAVE_IPV6
    else if (data_size == sizeof(struct in6_addr)) {
      var uintL index = 0;
      var object data = array_displace_check(arg,vec_len,&index);
      begin_system_call();
      he = gethostbyaddr((char*)(TheSbvector(data)->data+index),
                         data_size,AF_INET6);
      end_system_call();
    }
   #endif
    else { resolve_host_bad_vector:
      pushSTACK(arg);                  /* TYPE-ERROR slot DATUM */
      pushSTACK(O(type_uint8_vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
     #ifdef HAVE_IPV6
      pushSTACK(fixnum(sizeof(struct in6_addr)));
     #endif
      pushSTACK(fixnum(sizeof(struct in_addr)));
      pushSTACK(arg); pushSTACK(TheSubr(subr_self)->name);
      error(type_error,
            #ifdef HAVE_IPV6
             GETTEXT("~S: IP address ~S must have length ~S or ~S")
            #else
             GETTEXT("~S: IP address ~S must have length ~S")
            #endif
             );
    }
  }
 #ifdef HAVE_IPV6
  else if ((fixnump(arg) && positivep(arg))
           || (bignump(arg) && positivep(arg)
               && Bignum_length(arg)*intDsize <= sizeof(struct in6_addr))) {
    /* arg is an integer of length at most 128 bit - IPv6 address */
    var struct in6_addr addr;
    UI_to_LEbytes(arg,8*sizeof(struct in6_addr),(uintB*)&addr);
    begin_system_call();
    he = gethostbyaddr((char*)&addr,sizeof(struct in6_addr),AF_INET6);
    end_system_call();
  }
 #endif
  else error_string_integer(arg);
  return he;
}


/* Look up a host's IP address, then call a user-defined function taking
   a `struct sockaddr' and its size, and returning a SOCKET. */
typedef SOCKET (*socket_connect_fn_t) (struct sockaddr * addr, int addrlen,
                                       void* opts);
local SOCKET with_host_port (const char* host, unsigned short port,
                             socket_connect_fn_t connector, void* opts) {
 #ifdef HAVE_IPV6
  {
    var struct sockaddr_in6 inaddr;
    FILL0(inaddr);
    if (inet_pton(AF_INET6,host,&inaddr.sin6_addr) > 0) {
      inaddr.sin6_family = AF_INET6;
      inaddr.sin6_port = htons(port);
      return connector((struct sockaddr *) &inaddr,
                       sizeof(struct sockaddr_in6), opts);
    }
  }
 #endif
  {
    var struct sockaddr_in inaddr;
    FILL0(inaddr);
    if (inet_pton(AF_INET,host,&inaddr.sin_addr) > 0) {
      inaddr.sin_family = AF_INET;
      inaddr.sin_port = htons(port);
      return connector((struct sockaddr *) &inaddr,
                       sizeof(struct sockaddr_in), opts);
    }
  }
  {
    var struct hostent * host_ptr; /* entry in hosts table */
    if ((host_ptr = gethostbyname(host)) == NULL) {
      errno = EINVAL; return INVALID_SOCKET; /* No such host! */
    }
    /* Check the address type for an internet host. */
   #ifdef HAVE_IPV6
    if (host_ptr->h_addrtype == AF_INET6) {
      /* Set up the socket data. */
      var struct sockaddr_in6 inaddr;
      FILL0(inaddr);
      inaddr.sin6_family = AF_INET6;
      inaddr.sin6_addr = *(struct in6_addr *)host_ptr->h_addr;
      inaddr.sin6_port = htons(port);
      return connector((struct sockaddr *) &inaddr,
                       sizeof(struct sockaddr_in6), opts);
    } else
   #endif
    if (host_ptr->h_addrtype == AF_INET) {
      /* Set up the socket data. */
      var struct sockaddr_in inaddr;
      FILL0(inaddr);
      inaddr.sin_family = AF_INET;
      inaddr.sin_addr = *(struct in_addr *)host_ptr->h_addr;
      inaddr.sin_port = htons(port);
      return connector((struct sockaddr *) &inaddr,
                       sizeof(struct sockaddr_in), opts);
    } else { /* Not an Internet host! */
      errno = EPROTOTYPE; return INVALID_SOCKET;
    }
  }
}

#endif /* TCPCONN */

/* ========================== X server connection ======================== */

#ifndef ENOSYS
  #define ENOSYS  EINVAL
#endif

#ifdef UNIXCONN
  #include <sys/un.h>  /* defines struct sockaddr_un */
  /* set X_UNIX_PATH and - on hpux only - OLD_UNIX_PATH */
  #ifndef X_UNIX_PATH
    #ifndef hpux
      #define X_UNIX_PATH "/tmp/.X11-unix/X"
    #else
      #define X_UNIX_PATH "/usr/spool/sockets/X11/"
      #define OLD_UNIX_PATH "/tmp/.X11-unix/X"
    #endif
  #endif
#endif

#ifdef TCPCONN
  #ifndef WIN32
    #ifdef HAVE_NETINET_TCP_H
      #if defined(__386BSD__) || defined(__NetBSD__)
        #include <machine/endian.h> /* needed for <netinet/tcp.h> */
      #endif
      #include <netinet/tcp.h> /* declares TCP_NODELAY */
    #endif
  #endif
#endif

#ifdef TCPCONN
local SOCKET connect_to_x_via_ip (struct sockaddr * addr, int addrlen,
                                  void* ignore) {
  var SOCKET fd;
  var int retries = 3; /* number of retries on ECONNREFUSED */
  (void)(ignore); /* no options -- ignore */
  do {
    if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) == INVALID_SOCKET)
      return INVALID_SOCKET;
   #ifdef TCP_NODELAY
    { /* turn off TCP coalescence (the bandwidth saving Nagle algorithm) */
      var int tmp = 1;
      setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (SETSOCKOPT_ARG_T)&tmp,
                 sizeof(int));
    }
   #endif
    /* Connect to the socket.
       If there is no X server or if the backlog has been reached,
       then ECONNREFUSED will be returned. */
    if (connect(fd, addr, addrlen) >= 0)
      break;
    saving_errno(CLOSE(fd));
    if (!(errno == ECONNREFUSED && (retries > 0)))
      return INVALID_SOCKET;
    sleep (1);
  } while (retries-- > 0);
  return fd;
}
#endif

#ifdef TCPCONN
  #define X_TCP_PORT  6000  /* from <X11/Xproto.h> */
#endif

/* connect_to_x_server(host,display)
 Attempts to connect to server, given host name and display number.
 Returns file descriptor (network socket). Returns -1 and sets errno
 if connection fails.
 An empty hostname is interpreted as the most efficient local connection to
 a server on the same machine (usually a UNIX domain socket).
 hostname="unix" is interpreted as a UNIX domain connection.
 To be used inside begin/end_system_call() only. */
global SOCKET connect_to_x_server (const char* host, int display)
{
  var SOCKET fd;         /* file descriptor to return */

  var int conntype; /* type of desired connection */
 #define conn_none 0
 #define conn_unix 1
 #define conn_tcp  2
 #ifdef TCPCONN
  conntype = conn_tcp;
 #else
  conntype = conn_none;
 #endif
 #ifdef UNIXCONN
  if (host[0] == '\0') {
   #ifndef apollo /* Unix domain sockets are *really* bad on apollos */
    conntype = conn_unix;
   #endif
  } else if (strcmp(host,"unix")==0) {
    conntype = conn_unix;
  }
 #endif

  /* Make the connection. Do retries in case server host has hit its
     backlog (which, unfortunately, isn't distinguishable from there not
     being a server listening at all, which is why we have to not retry
     too many times). */

 #ifdef UNIXCONN
  if (conntype == conn_unix) {
    var int retries = 3; /* number of retries on ECONNREFUSED */
    var struct sockaddr_un unaddr;          /* UNIX socket data block */
    var struct sockaddr * addr;             /* generic socket pointer */
    var int addrlen;                        /* length of addr */
    FILL0(unaddr);
   #ifdef hpux /* this is disgusting */
    var struct sockaddr_un ounaddr;         /* UNIX socket data block */
    var struct sockaddr * oaddr;            /* generic socket pointer */
    var int oaddrlen;                       /* length of addr */
    FILL0(unaddr);
   #endif

    unaddr.sun_family = AF_UNIX;
    sprintf (unaddr.sun_path, "%s%d", X_UNIX_PATH, display);
    addr = (struct sockaddr *) &unaddr;
   #ifdef HAVE_SOCKADDR_UN_LEN /* this is AIX */
    unaddr.sun_len = strlen(unaddr.sun_path); addrlen = sizeof(unaddr);
   #else
    addrlen = strlen(unaddr.sun_path) + sizeof(unaddr.sun_family);
   #endif
   #ifdef hpux /* this is disgusting */
    ounaddr.sun_family = AF_UNIX;
    sprintf (ounaddr.sun_path, "%s%d", OLD_UNIX_PATH, display);
    oaddr = (struct sockaddr *) &ounaddr;
    #ifdef HAVE_SOCKADDR_UN_LEN /* this is AIX */
    ounaddr.sun_len = strlen(ounaddr.sun_path); oaddrlen = sizeof(ounaddr);
    #else
    oaddrlen = strlen(ounaddr.sun_path) + sizeof(ounaddr.sun_family);
    #endif
   #endif

    /* Open the network connection. */
    do {
      if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0))
          != INVALID_SOCKET) {
        if (connect(fd, addr, addrlen) >= 0)
          break;
        else
          saving_errno(CLOSE(fd));
      }
     #ifdef hpux /* this is disgusting */
      if (errno == ENOENT) {
        if ((fd = socket((int) oaddr->sa_family, SOCK_STREAM, 0))
            != INVALID_SOCKET) {
          if (connect(fd, oaddr, oaddrlen) >= 0)
            break;
          else
            saving_errno(CLOSE(fd));
        }
      }
     #endif
      if (!((errno == ENOENT) && (retries > 0)))
        return INVALID_SOCKET;
      sleep (1);
    } while (retries-- > 0);
  }
  else
 #endif

   #ifdef TCPCONN
    if (conntype == conn_tcp) {
      var unsigned short port = X_TCP_PORT+display;
      if (host[0] == '\0') {
        var char host[MAXHOSTNAMELEN+1];
        get_hostname(host);
        fd = with_host_port(host,port,&connect_to_x_via_ip,NULL);
      } else {
        fd = with_host_port(host,port,&connect_to_x_via_ip,NULL);
      }
      if (fd == INVALID_SOCKET)
        return INVALID_SOCKET;
    }
    else
   #endif

      /* (conntype == conn_none) */
      {
        OS_set_errno(ENOSYS); return INVALID_SOCKET;
      }

  return fd;
}

/* ==================== General socket functions ======================= */

#ifdef SOCKET_STREAMS /* implies TCPCONN */

/* When calling getsockname(), getpeername(), accept(), we need room for
 either a sockaddr_in or a sockaddr_in6. Since we create only sockets
 with family AF_INET and AF_INET6, these are the only kinds of sockets
 we have to deal with (no sockaddr_ipx, sockaddr_un, etc.). */
typedef union {
  struct sockaddr_in inaddr;
 #ifdef HAVE_IPV6
  struct sockaddr_in6 inaddr6;
 #endif
} sockaddr_max_t;

/* socket_getlocalname_aux(socket_handle,hd)
 Return the IP name of the localhost for the given socket.
 Fills only hd->hostname and hd->port, not hd->truename.
 To be used inside begin/end_system_call() only. */
local host_data_t * socket_getlocalname_aux (SOCKET socket_handle,
                                             host_data_t * hd) {
  var sockaddr_max_t addr;
  var socklen_t addrlen = sizeof(sockaddr_max_t);
  FILL0(addr);
  if (getsockname(socket_handle,(struct sockaddr *)&addr,&addrlen) < 0)
    return NULL;
  /* Fill in hd->hostname and hd->port. */
  switch (((struct sockaddr *)&addr)->sa_family) {
   #ifdef HAVE_IPV6
    case AF_INET6:
      inet_ntop(AF_INET6,&addr.inaddr6.sin6_addr,hd->hostname,
                sizeof(hd->hostname));
      hd->port = ntohs(addr.inaddr6.sin6_port);
      break;
   #endif
    case AF_INET:
      inet_ntop(AF_INET,&addr.inaddr.sin_addr,hd->hostname,
                sizeof(hd->hostname));
      hd->port = ntohs(addr.inaddr.sin_port);
      break;
    default:                    /* AF_UNIX, AF_LOCAL &c */
      strcpy(hd->hostname,"localhost");
      hd->port = 0;
      break;
  }
  return hd;
}

/* socket_getlocalname(socket_handle,hd, truename_p)
 Return the IP name of the localhost for the given socket.
 Fills all of *hd, may even fetch truename.
 To be used inside begin/end_system_call() only. */
global host_data_t * socket_getlocalname (SOCKET socket_handle,
                                          host_data_t * hd, bool resolve_p) {
  if (socket_getlocalname_aux(socket_handle,hd) == NULL)
    return NULL;
  if (resolve_p) { /* Fill in hd->truename. */
    var char host[MAXHOSTNAMELEN+1];
    get_hostname(host);
    ASSERT(strlen(host) <= MAXHOSTNAMELEN);
    strcpy(hd->truename,host);
  } else {
    hd->truename[0] = '\0';
  }
  return hd;
}

/* socket_getpeername(socket_handle,hd,truename_p)
 Returns the name of the host to which IP socket fd is connected.
 Fills all of *hd, may even fetch truename.
 To be used inside begin/end_system_call() only. */
global host_data_t * socket_getpeername (SOCKET socket_handle,
                                         host_data_t * hd, bool resolve_p) {
  var sockaddr_max_t addr;
  var socklen_t addrlen = sizeof(sockaddr_max_t);
  var struct hostent* hp = NULL;
  FILL0(addr);
  /* Get host's IP address. */
  if (getpeername(socket_handle,(struct sockaddr *)&addr,&addrlen) < 0)
    return NULL;
  /* Fill in hd->port and hd->hostname, and retrieve hp. */
  switch (((struct sockaddr *)&addr)->sa_family) {
    #ifdef HAVE_IPV6
    case AF_INET6:
      inet_ntop(AF_INET6,&addr.inaddr6.sin6_addr,hd->hostname,
                sizeof(hd->hostname));
      hd->port = ntohs(addr.inaddr6.sin6_port);
      if (resolve_p)
        hp = gethostbyaddr((const char *)&addr.inaddr6.sin6_addr,
                           sizeof(struct in6_addr),AF_INET6);
      break;
    #endif
    case AF_INET:
      inet_ntop(AF_INET,&addr.inaddr.sin_addr,hd->hostname,
                sizeof(hd->hostname));
      hd->port = ntohs(addr.inaddr.sin_port);
      if (resolve_p)
        hp = gethostbyaddr((const char *)&addr.inaddr.sin_addr,
                           sizeof(struct in_addr),AF_INET);
      break;
    default:                    /* AF_UNIX, AF_LOCAL &c */
      strcpy(hd->hostname,"localhost");
      hd->port = 0;
      if (resolve_p)
        hp = gethostbyname(hd->hostname);
      break;
  }
  /* Fill in hd->truename. */
  if (hp) {
    ASSERT(strlen(hp->h_name) <= MAXHOSTNAMELEN);
    strcpy(hd->truename,hp->h_name);
  } else {
    hd->truename[0] = '\0';
  }
  return hd;
}

/* Creation of sockets on the server side:
 SOCKET socket_handle = create_server_socket_by_socket (&host_data, sock,
                                                        port, backlog);
 SOCKET socket_handle = create_server_socket_by_string (&host_data, interface,
                                                        port, backlog);
   both create a socket to which other processes can connect, one based on a
   existing socket, the other based on interface string (e.g., "0.0.0.0").
 SOCKET fd = accept_connection(socket_handle);
   waits for a connection to another process.
   This can (and should) be done multiple times for the same
   socket_handle. */
local SOCKET bindlisten_via_ip (struct sockaddr * addr, int addrlen,
                                void* backlog) {
  var SOCKET fd;
  /* Get a socket. */
  if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) == INVALID_SOCKET)
    return INVALID_SOCKET;
  /* Set an option for the next bind() call: Avoid an EADDRINUSE error
     in case there are TIME_WAIT or CLOSE_WAIT sockets hanging around on
     the port. (Sockets in LISTEN or ESTABLISHED state on the same port
     will still yield an error.) */
  {
    var unsigned int flag = 1;
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (SETSOCKOPT_ARG_T)&flag,
                   sizeof(flag)) < 0) {
      saving_errno(CLOSE(fd)); return INVALID_SOCKET;
    }
  }
  /* Bind it to the desired port. */
  if (bind(fd, addr, addrlen) >= 0)
    /* Start listening for client connections. */
    if (listen(fd, *(int *)backlog) >= 0)
      return fd;
  saving_errno(CLOSE(fd));
  return INVALID_SOCKET;
}

global SOCKET create_server_socket_by_string (host_data_t *hd,
                                              const char *ip_interface,
                                              unsigned int port,
                                              int backlog) {
  var SOCKET fd = with_host_port(ip_interface,(unsigned short)port,
                                 &bindlisten_via_ip,&backlog);
  if (fd == INVALID_SOCKET)
    return INVALID_SOCKET;
  /* Retrieve the assigned port. */
  if (socket_getlocalname_aux(fd,hd) != NULL)
    return fd;
  saving_errno(CLOSE(fd));
  return INVALID_SOCKET;
}

global SOCKET create_server_socket_by_socket (host_data_t *hd, SOCKET sock,
                                              unsigned int port, int backlog) {
  var SOCKET fd;
  var sockaddr_max_t addr;
  var socklen_t addrlen = sizeof(sockaddr_max_t);
  FILL0(addr);
  if (getsockname(sock,(struct sockaddr *)&addr,&addrlen) < 0)
    return INVALID_SOCKET;
  switch (((struct sockaddr *)&addr)->sa_family) {
   #ifdef HAVE_IPV6
    case AF_INET6:
      addr.inaddr6.sin6_port = htons(port);
      break;
   #endif
    case AF_INET:
      addr.inaddr.sin_port = htons(port);
      break;
    default: NOTREACHED;
  }
  fd = bindlisten_via_ip((struct sockaddr *)&addr,addrlen,&backlog);
  /* common part */
  if (fd == INVALID_SOCKET)
    return INVALID_SOCKET;
  /* Retrieve the assigned port. */
  if (socket_getlocalname_aux(fd,hd) != NULL)
    return fd;
  saving_errno(CLOSE(fd));
  return INVALID_SOCKET;
}

global SOCKET accept_connection (SOCKET socket_handle) {
 #if defined(WIN32_NATIVE)
  /* make it interruptible on windows
     it seems no need implementing interruptible_accept */
  if (!interruptible_socket_wait(socket_handle,socket_wait_read,NULL)) {
    WSASetLastError(WSAETIMEDOUT); /* user-inspired timeout */
    return INVALID_SOCKET;
  }
 #endif
  var sockaddr_max_t addr;
  var socklen_t addrlen = sizeof(sockaddr_max_t);
  FILL0(addr);
  return accept(socket_handle,(struct sockaddr *)&addr,&addrlen);
  /* We can ignore the contents of addr, because we can retrieve it again
     through socket_getpeername() later. */
}

/* Creation of sockets on the client side:
 SOCKET fd = create_client_socket(hostname,port,timeout);
   creates a connection to a server (which must be waiting
   on the specified host and port). */

local SOCKET connect_via_ip (struct sockaddr * addr, int addrlen,
                             void* timeout) {
  /* <http://cr.yp.to/docs/connect.html>:
     - make a non-blocking socket, connect(), select() for WR */
  var SOCKET fd;
  NO_BLOCK_DECL();
  if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) == INVALID_SOCKET)
    return INVALID_SOCKET;
 #if defined(HAVE_SELECT) || defined(WIN32_NATIVE)
  if (timeout) {
    START_NO_BLOCK(fd, goto fail);
  }
 #endif
  if (connect(fd, addr, addrlen) >= 0)
    return fd;
 #if defined(HAVE_SELECT) || defined(WIN32_NATIVE)
  if (errno == EINPROGRESS || errno == EWOULDBLOCK) {
    var struct timeval *tvp = (struct timeval*)timeout;
    if ((tvp == NULL) || (tvp->tv_sec != 0) || (tvp->tv_usec != 0)) { /*wait*/
     var int ret = 0;
     #if defined(WIN32_NATIVE)
      ret = interruptible_socket_wait(fd,socket_wait_write,tvp);
     #else
     restart_select: {
        var fd_set handle_set;
        FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
        ret = select(FD_SETSIZE,NULL,&handle_set,NULL,tvp);
        if (ret < 0) {
          if (errno == EINTR) goto restart_select;
          goto fail;
        }
       #if defined(SOL_SOCKET) && defined(SO_ERROR) && defined(HAVE_GETSOCKOPT)
        var int errorp;
        var socklen_t len = sizeof(errorp);
        if (getsockopt(fd,SOL_SOCKET,SO_ERROR,&errorp,&len) < 0) {
          goto fail;
        }
        if (errorp) {
          errno = errorp;
          goto fail;
        }
       #endif
      }
     #endif
      if (ret == 0) {
        errno = ETIMEDOUT;
        goto fail;
      }
    }
    if (timeout) { /* connected - restore blocking IO */
      END_NO_BLOCK(fd, goto fail);
      return fd;
    }
  }
 #endif
 fail:
  saving_errno(CLOSE(fd));
  return INVALID_SOCKET;
}

global SOCKET create_client_socket (const char* hostname, unsigned int port,
                                    void* timeout) {
  return with_host_port(hostname,(unsigned short)port,
                        &connect_via_ip,timeout);
}

#endif /* SOCKET_STREAMS */

#endif /* HAVE_GETHOSTBYNAME */

#endif /* UNIX || WIN32_NATIVE */
