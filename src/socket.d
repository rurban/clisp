# Setting up a connection to an X server, and other socket functions
# Bruno Haible 19.6.1994, 27.6.1997, 9.3.1999
# Marcus Daniels 28.9.1995, 9.9.1997

# The code in function connect_to_x_server originally came from the X11R5
# distribution, file mit/X/XConnDis.c, with the following modifications:
# - no support for DNETCONN or STREAMSCONN,
# - display name has already been split into hostname and display number,
# - doesn't return full host&display name and auth info,
# - doesn't depend on the X include files,
# - support IPv6.

# mit/X/XConnDis.c carries the following copyright notice:
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

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> # declares fcntl(), close(), sleep()
#endif

#include <errno.h>
extern int errno;

#include <stdio.h>   # declares sprintf()
#include <string.h>  # declares strcmp(), strlen(), strcpy()
#ifdef RETSTRLENTYPE /* unless strlen() is a macro */
  extern_C RETSTRLENTYPE strlen (STRLEN_CONST char* s);
#endif

# ================ hostnames and IP addresses only (no sockets) ================

#ifndef WIN32
  #ifdef HAVE_GETHOSTNAME
    extern_C int gethostname (char* name, GETHOSTNAME_SIZE_T namelen);
  #endif
  #ifdef HAVE_SYS_UTSNAME_H
    #include <sys/utsname.h>
    extern_C int uname (struct utsname * buf);
  #endif
#endif

# Fetches the machine's host name.
# get_hostname(host =);
# The name is allocated on the stack, with dynamic extent.
# < const char* host: The host name.
# (Note: In some cases we could get away with less system calls by simply
# setting
#   host = "localhost";
# But this seems not worth the trouble to think about it.)
# sds: never: you will always get localhost/127.0.0.1 - what's the point?
  #if defined(HAVE_GETHOSTNAME)
    #define get_hostname(host_assignment)  \
      var char hostname[MAXHOSTNAMELEN+1];                                \
      begin_system_call();                                                \
      if ( gethostname(&hostname[0],MAXHOSTNAMELEN) <0) { SOCK_error(); } \
      end_system_call();                                                  \
      hostname[MAXHOSTNAMELEN] = '\0';                                    \
      host_assignment &hostname[0];
  #elif defined(HAVE_SYS_UTSNAME_H)
    #define get_hostname(host_assignment)  \
      var struct utsname utsname;              \
      begin_system_call();                     \
      if ( uname(&utsname) <0) { OS_error(); } \
      end_system_call();                       \
      host_assignment &!utsname.nodename;
  #else
    #define get_hostname(host_assignment)  \
      ??
  #endif

#ifndef WIN32
  #if defined(UNIXCONN) || defined(TCPCONN)
    # include <sys/types.h>
    #include <sys/socket.h> # declares socket(), connect(), setsockopt(), defines AF_UNIX, AF_INET, AF_INET6
  #endif
  #if defined(TCPCONN)
    #include <netinet/in.h> # declares htons(), defines struct sockaddr_in
    #ifdef HAVE_ARPA_INET_H
      #include <arpa/inet.h> # declares inet_addr() and maybe inet_pton(), inet_ntop()
    #endif
    #ifdef IPV6_NEED_LINUX_IN6_H
      #include <linux/in6.h> # defines struct in6_addr, struct sockaddr_in6
    #endif
  #endif
  #ifdef HAVE_GETHOSTBYNAME
    # include <sys/types.h>
    #ifdef HAVE_NETDB_H
      # include <sys/socket.h>
      # include <netdb.h>
    #else
      # include <sun/netdb.h>
    #endif
    extern_C struct hostent * gethostbyname (GETHOSTBYNAME_CONST char* name);
  #endif
#endif

# Converts an AF_INET address to a printable, presentable format.
# ipv4_ntop(buffer,addr);
# > sockaddr_in addr: IPv4 address
# < char[] buffer: printable address
# buffer should have at least 15+1 characters.
#ifdef HAVE_IPV4
  #ifdef HAVE_INET_NTOP
    #define ipv4_ntop(buffer,addr)  \
      inet_ntop(AF_INET,&addr,buffer,15+1);
  #else
    #define ipv4_ntop(buffer,addr)  \
      strcpy(buffer,inet_ntoa(addr));
  #endif
#endif

# Converts an AF_INET6 address to a printable, presentable format.
# ipv6_ntop(buffer,addr);
# > sockaddr_in6 addr: IPv6 address
# < char[] buffer: printable address
# buffer should have at least 45+1 characters.
#ifdef HAVE_IPV6
  #ifdef HAVE_INET_NTOP
    #define ipv6_ntop(buffer,addr)  \
      inet_ntop(AF_INET6,&addr,buffer,45+1);
  #else
    #define ipv6_ntop(buffer,addr)  \
      sprintf(buffer,"%x:%x:%x:%x:%x:%x:%x:%x", \
              ntohs((addr).in6_u.u6_addr16[0]), \
              ntohs((addr).in6_u.u6_addr16[1]), \
              ntohs((addr).in6_u.u6_addr16[2]), \
              ntohs((addr).in6_u.u6_addr16[3]), \
              ntohs((addr).in6_u.u6_addr16[4]), \
              ntohs((addr).in6_u.u6_addr16[5]), \
              ntohs((addr).in6_u.u6_addr16[6]), \
              ntohs((addr).in6_u.u6_addr16[7])  \
             );
  #endif
#endif

#ifdef MACHINE_KNOWN

LISPFUNN(machine_instance,0)
# (MACHINE-INSTANCE), CLTL S. 447
  { var object result = O(machine_instance_string);
    if (nullp(result)) # noch unbekannt?
      { # ja -> Hostname abfragen und dessen Internet-Adresse holen:
        # (let* ((hostname (unix:gethostname))
        #        (address (unix:gethostbyname hostname)))
        #   (if (or (null address) (zerop (length address)))
        #     hostname
        #     (apply #'string-concat hostname " [" (inet-ntop address) "]")
        # ) )
        var const char* host;
        get_hostname(host =);
        result = asciz_to_string(host,O(misc_encoding)); # Hostname als Ergebnis
        #ifdef HAVE_GETHOSTBYNAME
          pushSTACK(result); # Hostname als 1. String
          { var uintC stringcount = 1;
            # Internet-Information holen:
            var struct hostent * h;
            begin_system_call();
            h = gethostbyname(host);
            end_system_call();
            if (!(h == (struct hostent *)NULL)
                && !(h->h_addr == (char*)NULL)
                && (h->h_length > 0)
               )
              {
                #ifdef HAVE_IPV6
                if (h->h_addrtype == AF_INET6)
                  { var char buffer[45+1];
                    ipv6_ntop(buffer,*(const struct in6_addr*)h->h_addr);
                    pushSTACK(ascii_to_string(" ["));
                    pushSTACK(asciz_to_string(buffer,O(misc_encoding)));
                    pushSTACK(ascii_to_string("]"));
                    stringcount += 3;
                  }
                else
                #endif
                if (h->h_addrtype == AF_INET)
                  { var char buffer[15+1];
                    ipv4_ntop(buffer,*(const struct in_addr*)h->h_addr);
                    pushSTACK(ascii_to_string(" ["));
                    pushSTACK(asciz_to_string(buffer,O(misc_encoding)));
                    pushSTACK(ascii_to_string("]"));
                    stringcount += 3;
                  }
              }
            # Strings zusammenhängen:
            result = string_concat(stringcount);
          }
        #endif
        # Das Ergebnis merken wir uns für's nächste Mal:
        O(machine_instance_string) = result;
      }
    value1 = result; mv_count=1;
  }

#endif # MACHINE_KNOWN

# We assume that if we have gethostbyname(), we have a networking OS
# (Unix or Win32).
#ifdef HAVE_GETHOSTBYNAME

# ========================= General socket utilities =========================

#if defined(UNIXCONN) || defined(TCPCONN)

#ifndef WIN32
  # include <sys/socket.h>
  extern_C SOCKET socket (int domain, int type, int protocol);
  #ifndef __GLIBC__ # glibc2 has a very weird declaration of connect(), autoconf cannot help
    extern_C int connect (SOCKET fd, CONNECT_CONST CONNECT_NAME_T name, CONNECT_ADDRLEN_T namelen);
  #endif
  extern_C int setsockopt (SOCKET fd, int level, int optname, SETSOCKOPT_CONST SETSOCKOPT_ARG_T optval, SETSOCKOPT_OPTLEN_T optlen);
#endif

# A wrapper around the closesocket() function/macro.
  #define CLOSESOCKET(fd)  while ((closesocket(fd) < 0) && sock_errno_is(EINTR)) ;

# A wrapper around the connect() function.
  global int nonintr_connect (SOCKET fd, struct sockaddr * name, int namelen);
  global int nonintr_connect(fd,name,namelen)
    var SOCKET fd;
    var struct sockaddr * name;
    var int namelen;
    { var int retval;
      do { retval = connect(fd,name,namelen); }
         while ((retval < 0) && sock_errno_is(EINTR));
      return retval;
    }
  #undef connect  # wg. UNIX_CYGWIN32
  #define connect nonintr_connect

# Execute a statement, but save sock_errno during it.
# saving_sock_errno(statement);
  #ifdef WIN32
    #define saving_sock_errno(statement)  \
      { int _olderrno = WSAGetLastError(); statement; WSASetLastError(_olderrno); }
  #else
    #define saving_sock_errno(statement)  \
      { int _olderrno = errno; statement; errno = _olderrno; }
  #endif

#endif # UNIXCONN || TCPCONN

#if defined(TCPCONN)

#ifndef WIN32
  extern_C RET_INET_ADDR_TYPE inet_addr (INET_ADDR_CONST char* host);
#endif

#if !defined(HAVE_INET_PTON)
# Newer RPCs specify that FQDNs can start with a digit, but can never consist
# only of digits and dots, because of the top level domain. Use this criterion
# to distinguish possible IP addresses from FQDNs.
local boolean all_digits_dots (const char* host);
local boolean all_digits_dots(host)
  var const char* host;
  { until (*host == '\0')
      { var char c = *host++;
        if (!((c >= '0' && c <= '9') || (c == '.'))) return FALSE;
      }
    return TRUE;
  }
#endif

# Look up a host's IP address, then call a user-defined function taking
# a `struct sockaddr' and its size, and returning a SOCKET.
typedef SOCKET (*socket_connect_fn) (struct sockaddr * addr, int addrlen);
local SOCKET with_hostname (const char* host, unsigned short port, socket_connect_fn connector);
local SOCKET with_hostname(host,port,connector)
  var const char* host;
  var unsigned short port;
  var socket_connect_fn connector;
  {
    #ifdef HAVE_INET_PTON
    #ifdef HAVE_IPV6
    { var struct sockaddr_in6 inaddr;
      if (inet_pton(AF_INET6,host,&inaddr.sin6_addr) > 0)
        { inaddr.sin6_family = AF_INET6;
          inaddr.sin6_port = htons(port);
          return connector((struct sockaddr *) &inaddr, sizeof(struct sockaddr_in6));
    }   }
    #endif
    { var struct sockaddr_in inaddr;
      if (inet_pton(AF_INET,host,&inaddr.sin_addr) > 0)
        { inaddr.sin_family = AF_INET;
          inaddr.sin_port = htons(port);
          return connector((struct sockaddr *) &inaddr, sizeof(struct sockaddr_in));
    }   }
    #else
    # if numeric host name then try to parse it as such; do the number check
    # first because some systems return garbage instead of INVALID_INETADDR
    if (all_digits_dots(host))
      { var struct sockaddr_in inaddr;
        var uint32 hostinetaddr = inet_addr(host) INET_ADDR_SUFFIX ;
        if (!(hostinetaddr == ((uint32) -1)))
          { inaddr.sin_family = AF_INET;
            inaddr.sin_addr.s_addr = hostinetaddr;
            inaddr.sin_port = htons(port);
            return connector((struct sockaddr *) &inaddr, sizeof(struct sockaddr_in));
      }   }
    #endif
    { var struct hostent * host_ptr; # entry in hosts table
      if ((host_ptr = gethostbyname(host)) == NULL)
        { sock_set_errno(EINVAL); return INVALID_SOCKET; } # No such host!
      # Check the address type for an internet host.
      #ifdef HAVE_IPV6
      if (host_ptr->h_addrtype == AF_INET6)
        { # Set up the socket data.
          var struct sockaddr_in6 inaddr;
          inaddr.sin6_family = AF_INET6;
          inaddr.sin6_addr = *(struct in6_addr *)host_ptr->h_addr;
          inaddr.sin6_port = htons(port);
          return connector((struct sockaddr *) &inaddr, sizeof(struct sockaddr_in6));
        }
      else
      #endif
      if (host_ptr->h_addrtype == AF_INET)
        { # Set up the socket data.
          var struct sockaddr_in inaddr;
          inaddr.sin_family = AF_INET;
          inaddr.sin_addr = *(struct in_addr *)host_ptr->h_addr;
          inaddr.sin_port = htons(port);
          return connector((struct sockaddr *) &inaddr, sizeof(struct sockaddr_in));
        }
      else
        { sock_set_errno(EPROTOTYPE); return INVALID_SOCKET; } # Not an Internet host!
    }
  }

#endif # TCPCONN

# ========================== X server connection ========================

# Attempts to connect to server, given host name and display number.
# Returns file descriptor (network socket). Returns -1 and sets errno
# if connection fails.
# An empty hostname is interpreted as the most efficient local connection to
# a server on the same machine (usually a UNIX domain socket).
# hostname="unix" is interpreted as a UNIX domain connection.

global SOCKET connect_to_x_server (const char* host, int display);

#ifndef ENOSYS
  #define ENOSYS  EINVAL
#endif

#ifndef WIN32
  #include <fcntl.h> # declares fcntl() and defines F_SETFD
  #ifdef FCNTL_DOTS
    extern_C int fcntl (int fd, int cmd, ...);
  #else
    extern_C int fcntl (int fd, int cmd, int arg);
  #endif
  #ifndef FD_CLOEXEC
    #define FD_CLOEXEC  1
  #endif
#endif

#if defined(UNIXCONN) || defined(TCPCONN)
  extern_C unsigned int sleep (unsigned int seconds);
#endif

#ifdef UNIXCONN
  #include <sys/un.h>  # defines struct sockaddr_un
  # set X_UNIX_PATH and - on hpux only - OLD_UNIX_PATH
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
        #include <machine/endian.h> # needed for <netinet/tcp.h>
      #endif
      #include <netinet/tcp.h> # declares TCP_NODELAY
    #endif
  #endif
#endif

#ifdef TCPCONN
local SOCKET connect_to_x_via_ip (struct sockaddr * addr, int addrlen);
local SOCKET connect_to_x_via_ip(addr,addrlen)
  var struct sockaddr * addr;
  var int addrlen;
  { var SOCKET fd;
    var int retries = 3; # number of retries on ECONNREFUSED
    do { if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) == INVALID_SOCKET)
           { return INVALID_SOCKET; }
         #ifdef TCP_NODELAY
         # turn off TCP coalescence (the bandwidth saving Nagle algorithm)
         { int tmp = 1;
           setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (SETSOCKOPT_ARG_T)&tmp, sizeof(int));
         }
         #endif
         # Connect to the socket.
         # If there is no X server or if the backlog has been reached,
         # then ECONNREFUSED will be returned.
         if (connect(fd, addr, addrlen) >= 0)
           break;
         saving_sock_errno(CLOSESOCKET(fd));
         if (!(sock_errno_is(ECONNREFUSED) && (retries > 0)))
           { return INVALID_SOCKET; }
         sleep (1);
       }
       while (retries-- > 0);
    return fd;
  }
#endif

#ifdef TCPCONN
  #define X_TCP_PORT  6000  # from <X11/Xproto.h>
#endif

global SOCKET connect_to_x_server(host,display)
  var const char* host;  # host of display
  var int display;       # display number (screen number always zero)
{ var SOCKET fd;         # file descriptor to return

  var int conntype; # type of desired connection
  #define conn_none 0
  #define conn_unix 1
  #define conn_tcp  2
  #ifdef TCPCONN
    conntype = conn_tcp;
  #else
    conntype = conn_none;
  #endif
  #ifdef UNIXCONN
    if (host[0] == '\0')
      {
        #ifndef apollo # Unix domain sockets are *really* bad on apollos
        conntype = conn_unix;
        #endif
      }
    else if (strcmp(host,"unix")==0)
      { conntype = conn_unix; }
  #endif

  # Make the connection. Do retries in case server host has hit its
  # backlog (which, unfortunately, isn't distinguishable from there not
  # being a server listening at all, which is why we have to not retry
  # too many times).

  #ifdef UNIXCONN
  if (conntype == conn_unix)
    {
      var int retries = 3; # number of retries on ECONNREFUSED
      var struct sockaddr_un unaddr;          # UNIX socket data block
      var struct sockaddr * addr;             # generic socket pointer
      var int addrlen;                        # length of addr
      #ifdef hpux /* this is disgusting */
      var struct sockaddr_un ounaddr;         # UNIX socket data block
      var struct sockaddr * oaddr;            # generic socket pointer
      var int oaddrlen;                       # length of addr
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

      # Open the network connection.
      do { if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) != INVALID_SOCKET)
             { if (connect(fd, addr, addrlen) >= 0)
                 break;
                 else
                 saving_sock_errno(CLOSESOCKET(fd));
             }
           #ifdef hpux /* this is disgusting */
           if (errno == ENOENT)
             { if ((fd = socket((int) oaddr->sa_family, SOCK_STREAM, 0)) != INVALID_SOCKET)
                 { if (connect(fd, oaddr, oaddrlen) >= 0)
                     break;
                     else
                     saving_sock_errno(CLOSESOCKET(fd));
             }   }
           #endif
           if (!((errno == ENOENT) && (retries > 0)))
             { return INVALID_SOCKET; }
           sleep (1);
         }
         while (retries-- > 0);
    }
  else
  #endif

  #ifdef TCPCONN
  if (conntype == conn_tcp)
    {
      var unsigned short port = X_TCP_PORT+display;
      if (host[0] == '\0')
        { get_hostname(host =);
          fd = with_hostname(host,port,&connect_to_x_via_ip);
        }
        else
        { fd = with_hostname(host,port,&connect_to_x_via_ip); }
      if (fd == INVALID_SOCKET) { return INVALID_SOCKET; }
    }
  else
  #endif

  # (conntype == conn_none)
    { errno = ENOSYS; return INVALID_SOCKET; }

  #ifndef WIN32
    # Set close-on-exec so that we won't get confused if we fork().
    fcntl(fd,F_SETFD,FD_CLOEXEC);
  #endif

  return fd;
}

# ====================== General socket functions =========================

#ifdef SOCKET_STREAMS # implies TCPCONN

# When calling getsockname(), getpeername(), accept(), we need room for
# either a sockaddr_in or a sockaddr_in6. Since we create only sockets
# with family AF_INET and AF_INET6, these are the only kinds of sockets
# we have to deal with (no sockaddr_ipx, sockaddr_un, etc.).
typedef union {
                struct sockaddr_in inaddr;
                #ifdef HAVE_IPV6
                struct sockaddr_in6 inaddr6;
                #endif
              }
        sockaddr_max;

# Auxiliary function:
# socket_getlocalname(socket_handle,hd)
# socket_getlocalname_aux(socket_handle,hd)
# return the IP name of the localhost for the given socket.

# Fills only hd->hostname and hd->port, not hd->truename.
local host_data * socket_getlocalname_aux (SOCKET socket_handle, host_data * hd);
local host_data * socket_getlocalname_aux(socket_handle,hd)
  var SOCKET socket_handle;
  var host_data * hd;
  {
    var sockaddr_max addr;
    var int addrlen = sizeof(sockaddr_max);
    if (getsockname(socket_handle,(struct sockaddr *)&addr,&addrlen) < 0)
      return NULL;
    # Fill in hd->hostname and hd->port.
    switch (((struct sockaddr *)&addr)->sa_family)
      {
        #ifdef HAVE_IPV6
        case AF_INET6:
          ipv6_ntop(hd->hostname,addr.inaddr6.sin6_addr);
          hd->port = ntohs(addr.inaddr6.sin6_port);
          break;
        #endif
        case AF_INET:
          ipv4_ntop(hd->hostname,addr.inaddr.sin_addr);
          hd->port = ntohs(addr.inaddr.sin_port);
          break;
        default: NOTREACHED
      }
    return hd;
  }

# Fills all of *hd.
global host_data * socket_getlocalname (SOCKET socket_handle, host_data * hd);
global host_data * socket_getlocalname(socket_handle,hd)
  var SOCKET socket_handle;
  var host_data * hd;
  {
    if (socket_getlocalname_aux(socket_handle,hd) == NULL)
      return NULL;
    # Fill in hd->truename.
    {
      var const char* host;
      get_hostname(host =); # was: host = "localhost";
      ASSERT(strlen(host) <= MAXHOSTNAMELEN);
      strcpy(hd->truename,host);
    }
    return hd;
  }

# Auxiliary function:
# socket_getpeername (socket_handle, hd)
# returns the name of the host to which IP socket fd is connected.

# Fills all of *hd.
global host_data * socket_getpeername (SOCKET socket_handle, host_data * hd);
global host_data * socket_getpeername(socket_handle,hd)
  var SOCKET socket_handle;
  var host_data * hd;
  {
    var sockaddr_max addr;
    var int addrlen = sizeof(sockaddr_max);
    var struct hostent* hp;
    # Get host's IP address.
    if (getpeername(socket_handle,(struct sockaddr *)&addr,&addrlen) < 0)
      return NULL;
    # Fill in hd->port and hd->hostname, and retrieve hp.
    switch (((struct sockaddr *)&addr)->sa_family)
      {
        #ifdef HAVE_IPV6
        case AF_INET6:
          ipv6_ntop(hd->hostname,addr.inaddr6.sin6_addr);
          hd->port = ntohs(addr.inaddr6.sin6_port);
          hp = gethostbyaddr((const char *)&addr.inaddr6.sin6_addr,sizeof(struct in6_addr),AF_INET6);
          break;
        #endif
        case AF_INET:
          ipv4_ntop(hd->hostname,addr.inaddr.sin_addr);
          hd->port = ntohs(addr.inaddr.sin_port);
          hp = gethostbyaddr((const char *)&addr.inaddr.sin_addr,sizeof(struct in_addr),AF_INET);
          break;
        default: NOTREACHED
      }
    # Fill in hd->truename.
    if (hp)
      { ASSERT(strlen(hp->h_name) <= MAXHOSTNAMELEN);
        strcpy(hd->truename,hp->h_name);
      }
      else
      { hd->truename[0] = '\0'; }
    return hd;
  }

# Creation of sockets on the server side:
# SOCKET socket_handle = create_server_socket (&host_data, sock, port);
#   creates a socket to which other processes can connect.
# SOCKET fd = accept_connection(socket_handle);
#   waits for a connection to another process.
#   This can (and should) be done multiple times for the same
#   socket_handle.

global SOCKET create_server_socket (host_data *hd, SOCKET sock, unsigned int port);
local SOCKET bindlisten_via_ip (struct sockaddr * addr, int addrlen);
local SOCKET bindlisten_via_ip(addr,addrlen)
  var struct sockaddr * addr;
  var int addrlen;
  { var SOCKET fd;
    # Get a socket.
    if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) == INVALID_SOCKET)
      { return INVALID_SOCKET; }
    # Set an option for the next bind() call: Avoid an EADDRINUSE error
    # in case there are TIME_WAIT or CLOSE_WAIT sockets hanging around on
    # the port. (Sockets in LISTEN or ESTABLISHED state on the same port
    # will still yield an error.)
    { var unsigned int flag = 1;
      if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (SETSOCKOPT_ARG_T)&flag, sizeof(flag)) < 0)
        { saving_sock_errno(CLOSESOCKET(fd)); return INVALID_SOCKET; }
    }
    # Bind it to the desired port.
    if (bind(fd, addr, addrlen) >= 0)
      # Start listening for client connections.
      if (listen(fd, 1) >= 0)
        { return fd; }
    saving_sock_errno(CLOSESOCKET(fd));
    return INVALID_SOCKET;
  }
global SOCKET create_server_socket (hd, sock, port)
  var host_data *hd;
  var SOCKET sock;
  var unsigned int port;
  {
    var SOCKET fd;
    if (sock == INVALID_SOCKET)
      {
        var const char* host;
        get_hostname(host =); # was: host = "localhost";
        fd = with_hostname(host,port,&bindlisten_via_ip);
      }
      else
      {
        var sockaddr_max addr;
        var int addrlen = sizeof(sockaddr_max);
        if (getsockname(sock,(struct sockaddr *)&addr,&addrlen) < 0)
          return INVALID_SOCKET;
        switch (((struct sockaddr *)&addr)->sa_family)
          {
            #ifdef HAVE_IPV6
            case AF_INET6:
              addr.inaddr6.sin6_port = htons(0);
              break;
            #endif
            case AF_INET:
              addr.inaddr.sin_port = htons(0);
              break;
            default: NOTREACHED
          }
        fd = bindlisten_via_ip((struct sockaddr *)&addr,addrlen);
      }
    if (fd == INVALID_SOCKET)
      return INVALID_SOCKET;
    # Retrieve the assigned port.
    if (socket_getlocalname_aux(fd,hd) != NULL)
      return fd;
    saving_sock_errno(CLOSESOCKET(fd));
    return INVALID_SOCKET;
  }

global SOCKET accept_connection (SOCKET socket_handle);
global SOCKET accept_connection (socket_handle)
  var SOCKET socket_handle;
  {
    var sockaddr_max addr;
    var int addrlen = sizeof(sockaddr_max);
    return accept(socket_handle,(struct sockaddr *)&addr,&addrlen);
    # We can ignore the contents of addr, because we can retrieve it again
    # through socket_getpeername() later.
  }

# Creation of sockets on the client side:
# SOCKET fd = create_client_socket(hostname,port);
#   creates a connection to a server (which must be waiting
#   on the specified host and port).

global SOCKET create_client_socket (const char* hostname, unsigned int port);
local SOCKET connect_via_ip (struct sockaddr * addr, int addrlen);
local SOCKET connect_via_ip(addr,addrlen)
  var struct sockaddr * addr;
  var int addrlen;
  { var SOCKET fd;
    if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) == INVALID_SOCKET)
      { return INVALID_SOCKET; }
    if (connect(fd, addr, addrlen) >= 0)
      { return fd; }
    saving_sock_errno(CLOSESOCKET(fd));
    return INVALID_SOCKET;
  }
global SOCKET create_client_socket(hostname,port)
  var const char* hostname;
  var unsigned int port;
  {
    return with_hostname(hostname,port,&connect_via_ip);
  }

# ==================== miscellaneous network related stuff ====================

# Lisp interface to getservbyport(3) and getservbyname(3)

#define ARR_TO_LIST(val,test,expr)                      \
  { int ii; for (ii = 0; test; ii ++) { pushSTACK(expr); } val = listof(ii); }

#define SERVENT_TO_STACK(se)                                                \
  { var object tmp;                                                         \
    pushSTACK(asciz_to_string(se->s_name,Symbol_value(S(ascii))));          \
    ARR_TO_LIST(tmp,(se->s_aliases[ii] != NULL),                            \
                asciz_to_string(se->s_aliases[ii],Symbol_value(S(ascii)))); \
    pushSTACK(tmp);                                                         \
    pushSTACK(L_to_I(ntohs(se->s_port)));                                   \
    pushSTACK(asciz_to_string(se->s_proto,Symbol_value(S(ascii))));         \
  }

LISPFUN(socket_service_port,0,2,norest,nokey,0,NIL)
# (LISP:SOCKET-SERVICE-PORT &optional service-name protocol)
# NB: Previous versions of this functions
#  - accepted a string containing a number, e.g. "80",
#  - returned NIL when the port does not belong to a named service.
# Sam has changed this. Ask him why.
# sds: for consistency: `service-name': string ==> name; number ==> port number
#      when there is no service associated with a name or a number, an error is
#      signalled, so that whenever this function returns, you can be sure it
#      returns something useful.
{
  var object protocol = popSTACK();
  var object serv = popSTACK();
  var struct servent * se;
  var const char * proto;

  if (eq(protocol,unbound) || nullp(protocol))
    proto = "tcp";
  else if (stringp(protocol))
    proto = TheAsciz(string_to_asciz(protocol,Symbol_value(S(ascii))));
  else
    fehler_string(protocol);

  if (eq(serv,unbound) || eq(serv,S(Kdefault)) || nullp(serv)) {
    var uintL count = 0;
    #ifndef WIN32
    begin_system_call();
    for (; (se = getservent()); count++) {
      end_system_call();
      SERVENT_TO_STACK(se);
      pushSTACK(vectorof(4));
      begin_system_call();
    }
    endservent();
    end_system_call();
    #else # WIN32 does not have getservent()
    var uintL port;
    begin_system_call();
    for (port = 0; port < 0x10000; port++) {
      se = getservbyport(port,proto);
      if (!(se==NULL)) {
        end_system_call();
        SERVENT_TO_STACK(se);
        pushSTACK(vectorof(4));
        begin_system_call();
      }
    }
    end_system_call();
    #endif
    value1 = listof(count); mv_count = 1;
  } elif (stringp(serv)) {
    with_string_0(serv,Symbol_value(S(ascii)),serv_asciz,
      { begin_system_call();
        se = getservbyname(serv_asciz,proto);
        if (se==NULL) { OS_error(); }
        end_system_call();
      });
    SERVENT_TO_STACK(se);
    funcall(L(values),4);
  } elif (integerp(serv)) {
    var uintL port = I_to_UL(serv);
    begin_system_call();
    se = getservbyport(htons(port),proto);
    if (se==NULL) { OS_error(); }
    end_system_call();
    SERVENT_TO_STACK(se);
    funcall(L(values),4);
  } else
    fehler_string_integer(serv);
}

#endif # SOCKET_STREAMS

#ifdef EXPORT_SYSCALLS

# This piece of code is under the responsibility of Sam Steingold.

#define H_ERRMSG \
	(h_errno == HOST_NOT_FOUND ? "host not found" : \
	 (h_errno == TRY_AGAIN ? "try again later" : \
	  (h_errno == NO_RECOVERY ? "a non-recoverable error occurred" : \
	   (h_errno == NO_DATA ? "valid name, but no data for this host" : \
	    ((h_errno == NO_ADDRESS) || (h_errno == NO_DATA) ? \
	     "no IP address for this host" : "unknown error")))))

#define HOSTENT_TO_STACK(he)                                                  \
  { var object tmp;                                                           \
    pushSTACK(ascii_to_string(he->h_name));                                   \
    ARR_TO_LIST(tmp,(he->h_aliases[ii] != NULL),                              \
                asciz_to_string(he->h_aliases[ii],O(misc_encoding)));         \
    pushSTACK(tmp);                                                           \
    ARR_TO_LIST(tmp,(ii < he->h_length/sizeof(uint32)),                       \
                asciz_to_string(inet_ntop(he->h_addrtype,he->h_addr_list[ii], \
                                          buffer,MAXHOSTNAMELEN),             \
                                O(misc_encoding)));                           \
    pushSTACK(tmp);                                                           \
    pushSTACK(fixnum(he->h_addrtype));                                        \
  }

# Lisp interface to gethostbyname(3) and gethostbyaddr(3)
LISPFUN(resolve_host_ipaddr,0,1,norest,nokey,0,NIL)
# (LISP:RESOLVE-HOST-IPADDR &optional host)
{
  var object arg = popSTACK();
  var struct hostent *he = NULL;
  var char buffer[MAXHOSTNAMELEN];

  if (nullp(arg)) {
    int count = 0;
    begin_system_call();
    for (; (he = gethostent()); count++) {
      HOSTENT_TO_STACK(he);
      funcall(L(vector),4);
      pushSTACK(value1);
    }
    endhostent();
    end_system_call();
    value1 = listof(count); mv_count = 1;
    return;
  }

  if (eq(arg,unbound) || eq(arg,S(Kdefault))) {
    var char * host;
    get_hostname(host =);
    begin_system_call();
    he = gethostbyname(host);
    end_system_call();
  } else if (stringp(arg) || symbolp(arg)) {
    char * name = TheAsciz(string_to_asciz(stringp(arg)?arg:Symbol_name(arg),
                                           O(misc_encoding)));
    begin_system_call();
    #ifdef HAVE_INET_PTON
    if (inet_pton(AF_INET,name,(void*)buffer) > 0)
      he = gethostbyaddr(buffer,sizeof(struct in_addr),AF_INET);
    #ifdef HAVE_IPV6
    else if (inet_pton(AF_INET6,name,buffer) > 0)
      he = gethostbyaddr(buffer,sizeof(struct in6_addr),AF_INET);
    #endif # HAVE_IPV6
    #else # HAVE_INET_PTON
    if (all_digits_dots(name)) {
      var uint32 ip = inet_addr(name) INET_ADDR_SUFFIX;
      he = gethostbyaddr ((char*)&ip,sizeof(uint32),AF_INET);
    }
    #endif # HAVE_INET_PTON
    else he = gethostbyname(name);
    end_system_call();
  } else if (uint32_p(arg)) {
    var uint32 ip = htonl(I_to_UL(arg));
    begin_system_call();
    he = gethostbyaddr((char*)(&ip),sizeof(uint32),AF_INET);
    end_system_call();
  } else fehler_string_integer(arg);

  if (NULL == he) {
    pushSTACK(ascii_to_string(H_ERRMSG));
    pushSTACK(arg);
    fehler(os_error,
           GETTEXT("~: ~")
          );
  }

  HOSTENT_TO_STACK(he);
  funcall(L(values),4);
}

#endif # EXPORT_SYSCALLS

#endif # HAVE_GETHOSTBYNAME

#endif # UNIX || WIN32_NATIVE

