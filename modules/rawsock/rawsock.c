/*
 * Module for Raw Sockets / CLISP
 * Fred Cohen, 2003-2004
 * Don Cohen, 2003-2004
 * Sam Steingold 2004
 * <http://www.opengroup.org/onlinepubs/007908799/xns/syssocket.h.html>
 */

#include "config.h"

#include "clisp.h"

#if defined(HAVE_SYS_TYPES_H)
# include <sys/types.h>
#endif
#if defined(STDC_HEADERS)
# include <stdio.h>
# include <unistd.h>
#endif
#if defined(HAVE_SYS_SOCKET_H)
# include <sys/socket.h>
#endif
#if defined(HAVE_NETINET_IN_H)
# include <netinet/in.h>
#endif
#if defined(HAVE_ARPA_INET_H)
# include <arpa/inet.h>
#endif
#if defined(HAVE_LINUX_IF_PACKET_H)
# include <linux/if_packet.h>
#endif
#if defined(HAVE_NETINET_IF_ETHER_H)
# include <netinet/if_ether.h>
#endif
#if defined(HAVE_NET_IF_H)
# include <net/if.h>
#endif
#if defined(HAVE_SYS_IOCTL_H)
# include <sys/ioctl.h>
#endif
#if defined(HAVE_SYS_UN_H)
# include <sys/un.h>
#endif
#if defined(HAVE_ERRNO_H)
# include <errno.h>
#endif
#if defined(HAVE_STROPS_H)
# include <stropts.h>
#endif
#if defined(HAVE_POLL_H)
# include <poll.h>
#endif

DEFMODULE(rawsock,"RAWSOCK")

/* ================== helpers ================== */
/* can trigger GC */
static object my_check_type (object type, object datum) {
  pushSTACK(NIL);               /* no PLACE */
  pushSTACK(datum);             /* TYPE-ERROR slot DATUM */
  pushSTACK(type);              /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(type); pushSTACK(datum); pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,"~S: ~S is not of type ~S");
  return value1;
}
/* can trigger GC */
static object my_check_argument (object name, object datum) {
  pushSTACK(name); pushSTACK(datum); pushSTACK(TheSubr(subr_self)->name);
  check_value(error,"~S: ~S is not a valid ~S argument");
  return value1;
}
/* can trigger GC */
static object check_buffer_arg (object arg) {
  while(1) {
    if (missingp(arg)) return O(buffer);
    if (simple_bit_vector_p(Atype_8Bit,arg)) return arg;
    arg = my_check_type(S(simple_bit_vector),arg);
  }
}
/* can trigger GC */
object check_struct (object type, object arg) {
  while (!structurep(arg) ||
         nullp(memq(type,TheStructure(arg)->structure_types)))
    arg = my_check_type(type,arg);
  return arg;
}
/* DANGER: the reuturn value is invalidated by GC!
 can trigger GC */
void* check_struct_data (object type, object arg, size_t *size) {
  object vec = TheStructure(check_struct(type,arg))->recdata[1];
  *size = Sbvector_length(vec);
  return (void*)TheSbvector(vec)->data;
}

DEFVAR(buffer,`#.(MAKE-ARRAY 1518 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)))`);
DEFUN(RAWSOCK:BUFFER,) { VALUES1(O(buffer)); }
DEFUN(RAWSOCK:RESIZE-BUFFER,new-size) {
  /* new-size is already STACK_0 */
  pushSTACK(S(Kelement_type)); pushSTACK(`(UNSIGNED-BYTE 8)`);
  funcall(L(make_array),3);
  O(buffer)=value1;
}

DEFUN(RAWSOCK:SOCKADDR-FAMILY, sa) {
  size_t size;
  struct sockaddr *sa =
    (struct sockaddr*)check_struct_data(`RAWSOCK::SOCKADDR`,popSTACK(),&size);
  VALUES2(fixnum(sa->sa_family),fixnum(size));
}

/* can trigger GC */
static object make_sockaddr (void) {
  pushSTACK(allocate_bit_vector(Atype_8Bit,sizeof(struct sockaddr)));
  funcall(`RAWSOCK::MAKE-SA`,1);
  return value1;
}

DEFUN(RAWSOCK:MAKE-SOCKADDR,family data) {
  int family = check_socket_domain(STACK_1);
  struct sockaddr sa;
  unsigned char *buffer;
  size_t buffer_len;
  STACK_0 = check_buffer_arg(STACK_0);
  buffer_len = Sbvector_length(STACK_0);
  pushSTACK(allocate_bit_vector(Atype_8Bit,sizeof(sa.sa_family)+buffer_len));
  buffer = (void*)TheSbvector(STACK_0)->data;
  ((struct sockaddr*)buffer)->sa_family = family;
  begin_system_call();
  memcpy(((struct sockaddr*)buffer)->sa_data,TheSbvector(STACK_1)->data,
         buffer_len);
  end_system_call();
  funcall(`RAWSOCK::MAKE-SA`,1);
  skipSTACK(2);
}

/* invoke system call C, place return value in R, report error on socket C */
#define SYSCALL(r,s,c)                                  \
  do { begin_system_call(); r = c; end_system_call();   \
    if (r<0) {                                          \
      if (s<=0) OS_error();                             \
      else OS_file_error(fixnum(s));                    \
    }                                                   \
  } while(0)


/* ================== sys/socket.h interface ================== */

static int check_socket_domain (object arg) {
 restart_check_domain:
  if (!posfixnump(arg)) {
    if (nullp(arg)) return 0;
#  if defined(AF_UNSPEC)
    else if (eq(arg,`:AF_UNSPEC`))    return AF_UNSPEC;
#  endif
#  if defined(AF_UNIX)
    else if (eq(arg,`:AF_UNIX`))      return AF_UNIX;
#  endif
#  if defined(AF_LOCAL)
    else if (eq(arg,`:AF_LOCAL`))     return AF_LOCAL;
#  endif
#  if defined(AF_INET)
    else if (eq(arg,`:AF_INET`))      return AF_INET;
#  endif
#  if defined(AF_AX25)
    else if (eq(arg,`:AF_AX25`))      return AF_AX25;
#  endif
#  if defined(AF_IPX)
    else if (eq(arg,`:AF_IPX`))       return AF_IPX;
#  endif
#  if defined(AF_APPLETALK)
    else if (eq(arg,`:AF_APPLETALK`)) return AF_APPLETALK;
#  endif
#  if defined(AF_NETROM)
    else if (eq(arg,`:AF_NETROM`))    return AF_NETROM;
#  endif
#  if defined(AF_BRIDGE)
    else if (eq(arg,`:AF_BRIDGE`))    return AF_BRIDGE;
#  endif
#  if defined(AF_ATMPVC)
    else if (eq(arg,`:AF_ATMPVC`))    return AF_ATMPVC;
#  endif
#  if defined(AF_X25)
    else if (eq(arg,`:AF_X25`))       return AF_X25;
#  endif
#  if defined(AF_INET6)
    else if (eq(arg,`:AF_INET6`))     return AF_INET6;
#  endif
#  if defined(AF_ROSE)
    else if (eq(arg,`:AF_ROSE`))      return AF_ROSE;
#  endif
#  if defined(AF_DECnet)
    else if (eq(arg,`:AF_DECNET`))    return AF_DECnet;
#  endif
#  if defined(AF_NETBEUI)
    else if (eq(arg,`:AF_NETBEUI`))   return AF_NETBEUI;
#  endif
#  if defined(AF_SECURITY)
    else if (eq(arg,`:AF_SECURITY`))  return AF_SECURITY;
#  endif
#  if defined(AF_KEY)
    else if (eq(arg,`:AF_KEY`))       return AF_KEY;
#  endif
#  if defined(AF_NETLINK)
    else if (eq(arg,`:AF_NETLINK`))   return AF_NETLINK;
#  endif
#  if defined(AF_ROUTE)
    else if (eq(arg,`:AF_ROUTE`))     return AF_ROUTE;
#  endif
#  if defined(AF_PACKET)
    else if (eq(arg,`:AF_PACKET`))    return AF_PACKET;
#  endif
#  if defined(AF_ASH)
    else if (eq(arg,`:AF_ASH`))       return AF_ASH;
#  endif
#  if defined(AF_ECONET)
    else if (eq(arg,`:AF_ECONET`))    return AF_ECONET;
#  endif
#  if defined(AF_ATMSVC)
    else if (eq(arg,`:AF_ATMSVC`))    return AF_ATMSVC;
#  endif
#  if defined(AF_SNA)
    else if (eq(arg,`:AF_SNA`))       return AF_SNA;
#  endif
#  if defined(AF_IRDA)
    else if (eq(arg,`:AF_IRDA`))      return AF_IRDA;
#  endif
#  if defined(AF_PPPOX)
    else if (eq(arg,`:AF_PPPOX`))     return AF_PPPOX;
#  endif
#  if defined(AF_WANPIPE)
    else if (eq(arg,`:AF_WANPIPE`))   return AF_WANPIPE;
#  endif
#  if defined(AF_BLUETOOTH)
    else if (eq(arg,`:AF_BLUETOOTH`)) return AF_BLUETOOTH;
#  endif
    else {
      arg = my_check_argument(`DOMAIN`,arg);
      goto restart_check_domain;
    }
  } else return posfixnum_to_L(arg);
}
static int check_socket_type (object arg) {
 restart_check_type:
  if (!posfixnump(arg)) {
    if (nullp(arg)) return 0;
#  if defined(SOCK_STREAM)
    else if (eq(arg,`:SOCK_STREAM`))    return SOCK_STREAM;
#  endif
#  if defined(SOCK_DGRAM)
    else if (eq(arg,`:SOCK_DGRAM`))     return SOCK_DGRAM;
#  endif
#  if defined(SOCK_RAW)
    else if (eq(arg,`:SOCK_RAW`))       return SOCK_RAW;
#  endif
#  if defined(SOCK_RDM)
    else if (eq(arg,`:SOCK_RDM`))       return SOCK_RDM;
#  endif
#  if defined(SOCK_SEQPACKET)
    else if (eq(arg,`:SOCK_SEQPACKET`)) return SOCK_SEQPACKET;
#  endif
#  if defined(SOCK_PACKET)
    else if (eq(arg,`:SOCK_PACKET`))    return SOCK_PACKET;
#  endif
    else {
      arg = my_check_argument(`TYPE`,arg);
      goto restart_check_type;
    }
  }
  else return posfixnum_to_L(arg);
}
static int check_socket_protocol (object arg) {
 restart_check_protocol:
  if (!posfixnump(arg)) {
    if (nullp(arg)) return 0;
#  if defined(ETH_P_LOOP)
    else if (eq(arg,`:ETH_P_LOOP`))      return ETH_P_LOOP;
#  endif
#  if defined(ETH_P_PUP)
    else if (eq(arg,`:ETH_P_PUP`))       return ETH_P_PUP;
#  endif
#  if defined(ETH_P_PUPAT)
    else if (eq(arg,`:ETH_P_PUPAT`))     return ETH_P_PUPAT;
#  endif
#  if defined(ETH_P_IP)
    else if (eq(arg,`:ETH_P_IP`))        return ETH_P_IP;
#  endif
#  if defined(ETH_P_X25)
    else if (eq(arg,`:ETH_P_X25`))       return ETH_P_X25;
#  endif
#  if defined(ETH_P_ARP)
    else if (eq(arg,`:ETH_P_ARP`))       return ETH_P_ARP;
#  endif
#  if defined(ETH_P_BPQ)
    else if (eq(arg,`:ETH_P_BPQ`))       return ETH_P_BPQ;
#  endif
#  if defined(ETH_P_IEEEPUP)
    else if (eq(arg,`:ETH_P_IEEEPUP`))   return ETH_P_IEEEPUP;
#  endif
#  if defined(ETH_P_IEEEPUPAT)
    else if (eq(arg,`:ETH_P_IEEEPUPAT`)) return ETH_P_IEEEPUPAT;
#  endif
#  if defined(ETH_P_DEC)
    else if (eq(arg,`:ETH_P_DEC`))       return ETH_P_DEC;
#  endif
#  if defined(ETH_P_DNA_DL)
    else if (eq(arg,`:ETH_P_DNA_DL`))    return ETH_P_DNA_DL;
#  endif
#  if defined(ETH_P_DNA_RC)
    else if (eq(arg,`:ETH_P_DNA_RC`))    return ETH_P_DNA_RC;
#  endif
#  if defined(ETH_P_DNA_RT)
    else if (eq(arg,`:ETH_P_DNA_RT`))    return ETH_P_DNA_RT;
#  endif
#  if defined(ETH_P_LAT)
    else if (eq(arg,`:ETH_P_LAT`))       return ETH_P_LAT;
#  endif
#  if defined(ETH_P_DIAG)
    else if (eq(arg,`:ETH_P_DIAG`))      return ETH_P_DIAG;
#  endif
#  if defined(ETH_P_CUST)
    else if (eq(arg,`:ETH_P_CUST`))      return ETH_P_CUST;
#  endif
#  if defined(ETH_P_SCA)
    else if (eq(arg,`:ETH_P_SCA`))       return ETH_P_SCA;
#  endif
#  if defined(ETH_P_RARP)
    else if (eq(arg,`:ETH_P_RARP`))      return ETH_P_RARP;
#  endif
#  if defined(ETH_P_ATALK)
    else if (eq(arg,`:ETH_P_ATALK`))     return ETH_P_ATALK;
#  endif
#  if defined(ETH_P_AARP)
    else if (eq(arg,`:ETH_P_AARP`))      return ETH_P_AARP;
#  endif
#  if defined(ETH_P_IPX)
    else if (eq(arg,`:ETH_P_IPX`))       return ETH_P_IPX;
#  endif
#  if defined(ETH_P_IPV6)
    else if (eq(arg,`:ETH_P_IPV6`))      return ETH_P_IPV6;
#  endif
#  if defined(ETH_P_PPP_DISC)
    else if (eq(arg,`:ETH_P_PPP_DISC`))  return ETH_P_PPP_DISC;
#  endif
#  if defined(ETH_P_PPP_SES)
    else if (eq(arg,`:ETH_P_PPP_SES`))   return ETH_P_PPP_SES;
#  endif
#  if defined(ETH_P_ATMMPOA)
    else if (eq(arg,`:ETH_P_ATMMPOA`))   return ETH_P_ATMMPOA;
#  endif
#  if defined(ETH_P_ATMFATE)
    else if (eq(arg,`:ETH_P_ATMFATE`))   return ETH_P_ATMFATE;
#  endif
#  if defined(ETH_P_802_3)
    else if (eq(arg,`:ETH_P_802_3`))     return ETH_P_802_3;
#  endif
#  if defined(ETH_P_AX25)
    else if (eq(arg,`:ETH_P_AX25`))      return ETH_P_AX25;
#  endif
#  if defined(ETH_P_ALL)
    else if (eq(arg,`:ETH_P_ALL`))       return ETH_P_ALL;
#  endif
#  if defined(ETH_P_802_2)
    else if (eq(arg,`:ETH_P_802_2`))     return ETH_P_802_2;
#  endif
#  if defined(ETH_P_SNAP)
    else if (eq(arg,`:ETH_P_SNAP`))      return ETH_P_SNAP;
#  endif
#  if defined(ETH_P_DDCMP)
    else if (eq(arg,`:ETH_P_DDCMP`))     return ETH_P_DDCMP;
#  endif
#  if defined(ETH_P_WAN_PPP)
    else if (eq(arg,`:ETH_P_WAN_PPP`))   return ETH_P_WAN_PPP;
#  endif
#  if defined(ETH_P_PPP_MP)
    else if (eq(arg,`:ETH_P_PPP_MP`))    return ETH_P_PPP_MP;
#  endif
#  if defined(ETH_P_LOCALTALK)
    else if (eq(arg,`:ETH_P_LOCALTALK`)) return ETH_P_LOCALTALK;
#  endif
#  if defined(ETH_P_PPPTALK)
    else if (eq(arg,`:ETH_P_PPPTALK`))   return ETH_P_PPPTALK;
#  endif
#  if defined(ETH_P_TR_802_2)
    else if (eq(arg,`:ETH_P_TR_802_2`))  return ETH_P_TR_802_2;
#  endif
#  if defined(ETH_P_MOBITEX)
    else if (eq(arg,`:ETH_P_MOBITEX`))   return ETH_P_MOBITEX;
#  endif
#  if defined(ETH_P_CONTROL)
    else if (eq(arg,`:ETH_P_CONTROL`))   return ETH_P_CONTROL;
#  endif
#  if defined(ETH_P_IRDA)
    else if (eq(arg,`:ETH_P_IRDA`))      return ETH_P_IRDA;
#  endif
#  if defined(ETH_P_ECONET)
    else if (eq(arg,`:ETH_P_ECONET`))    return ETH_P_ECONET;
#  endif
    else {
      arg = my_check_argument(`PROTOCOL`,arg);
      goto restart_check_protocol;
    }
  } else return posfixnum_to_L(arg);
}

DEFUN(RAWSOCK:SOCKET,domain type protocol) {
  int sock;
  int protocol = check_socket_protocol(popSTACK());
  int type = check_socket_type(popSTACK());
  int domain = check_socket_domain(popSTACK());
  SYSCALL(sock,-1,socket(domain,type,protocol));
  VALUES1(fixnum(sock));
}

DEFUN(RAWSOCK:SOCKETPAIR,domain type protocol) {
  int sock[2], retval;
  int protocol = check_socket_protocol(popSTACK());
  int type = check_socket_type(popSTACK());
  int domain = check_socket_domain(popSTACK());
  SYSCALL(retval,-1,socketpair(domain,type,protocol,sock));
  VALUES2(fixnum(sock[0]),fixnum(sock[1]));
}

/* process optional (struct sockaddr*) argument:
   NIL: return NULL
   T: allocate
   SOCKADDR: extract data
 DANGER: the reuturn value is invalidated by GC!
 can trigger GC */
void optional_sockaddr_argument (gcv_object_t *arg, struct sockaddr**sa,
                                 size_t *size) {
  if (nullp(*arg)) *sa = NULL;
  else {
    if (eq(T,*arg)) *arg = make_sockaddr();
    *sa = (struct sockaddr*)check_struct_data(`RAWSOCK::SOCKADDR`,*arg,size);
  }
}

DEFUN(RAWSOCK:ACCEPT,socket sockaddr) {
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  struct sockaddr *sa = NULL;
  socklen_t sa_size;
  optional_sockaddr_argument(&STACK_0,&sa,&sa_size);
  /* no GC after this point! */
  SYSCALL(retval,sock,accept(sock,sa,&sa_size));
  VALUES3(fixnum(retval),fixnum(sa_size),STACK_0); skipSTACK(2);
}

DEFUN(RAWSOCK:BIND,socket sockaddr) {
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  size_t size;
  struct sockaddr *sa =
    (struct sockaddr*)check_struct_data(`RAWSOCK::SOCKADDR`,STACK_0,&size);
  /* no GC after this point! */
  SYSCALL(retval,sock,bind(sock,sa,size));
  VALUES0; skipSTACK(2);
}

DEFUN(RAWSOCK:CONNECT,socket sockaddr) {
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  size_t size;
  struct sockaddr *sa =
    (struct sockaddr*)check_struct_data(`RAWSOCK::SOCKADDR`,STACK_0,&size);
  /* no GC after this point! */
  SYSCALL(retval,sock,connect(sock,sa,size));
  VALUES0; skipSTACK(2);
}

DEFUN(RAWSOCK:GETPEERNAME,socket sockaddr) {
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  struct sockaddr *sa = NULL;
  socklen_t sa_size;
  optional_sockaddr_argument(&STACK_0,&sa,&sa_size);
  /* no GC after this point! */
  SYSCALL(retval,sock,getpeername(sock,sa,&sa_size));
  VALUES2(STACK_0,fixnum(sa_size)); skipSTACK(2);
}

DEFUN(RAWSOCK:GETSOCKNAME,socket sockaddr) {
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  struct sockaddr *sa = NULL;
  socklen_t sa_size;
  optional_sockaddr_argument(&STACK_0,&sa,&sa_size);
  /* no GC after this point! */
  SYSCALL(retval,sock,getsockname(sock,sa,&sa_size));
  VALUES2(STACK_0,fixnum(sa_size)); skipSTACK(2);
}

DEFUN(RAWSOCK:LISTEN,socket backlog) {
  int backlog = posfixnum_to_L(check_posfixnum(popSTACK()));
  int sock = posfixnum_to_L(check_posfixnum(popSTACK())), retval;
  SYSCALL(retval,sock,listen(sock,backlog));
  VALUES0;
}

#if defined(HAVE_POLL)
DEFUN(RAWSOCK:POLL,sockets) {
  skipSTACK(1);
  VALUES0;
}
#endif

/* ================== RECEIVING ================== */

/* remove 3 objects from the STACK and return the RECV flag
   based on MSG_PEEK MSG_OOB MSG_WAITALL */
int recv_flags (void) {
  int flags = 0
#  if defined(MSG_WAITALL)
    | (missingp(STACK_0) ? 0 : MSG_WAITALL)
#  endif
#  if defined(MSG_OOB)
    | (missingp(STACK_1) ? 0 : MSG_OOB)
#  endif
#  if defined(MSG_PEEK)
    | (missingp(STACK_2) ? 0 : MSG_PEEK)
#  endif
    ;
  skipSTACK(3);
  return flags;
}

DEFUN(RAWSOCK:RECV,socket buffer &key MSG_PEEK MSG_OOB MSG_WAITALL) {
  int flags = recv_flags();
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  void *buffer;
  size_t buffer_len;
  STACK_0 = check_buffer_arg(STACK_0);
  buffer = (void*)TheSbvector(STACK_1)->data;
  buffer_len = Sbvector_length(STACK_1);
  SYSCALL(retval,sock,recv(sock,buffer,buffer_len,flags));
  VALUES1(fixnum(retval)); skipSTACK(2);
}

DEFUN(RAWSOCK:RECVFROM, socket buffer address \
      &key MSG_PEEK MSG_OOB MSG_WAITALL) {
  int flags = recv_flags();
  int sock = posfixnum_to_L(check_posfixnum(STACK_2)), retval;
  struct sockaddr *sa = NULL;
  void *buffer;
  size_t buffer_len;
  socklen_t sa_size;
  STACK_1 = check_buffer_arg(STACK_1);
  optional_sockaddr_argument(&STACK_0,&sa,&sa_size);
  /* no GC after this point! */
  buffer = (void*)TheSbvector(STACK_1)->data;
  buffer_len = Sbvector_length(STACK_1);
  SYSCALL(retval,sock,recvfrom(sock,buffer,buffer_len,flags,sa,&sa_size));
  VALUES3(fixnum(retval),fixnum(sa_size),STACK_0); skipSTACK(3);
}

DEFUN(RAWSOCK:RECVMSG,socket message &key MSG_PEEK MSG_OOB MSG_WAITALL) {
  int flags = recv_flags();
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  size_t size;
  struct msghdr *message =
    (struct msghdr*)check_struct_data(`RAWSOCK::MSGHDR`,STACK_0,&size);
  SYSCALL(retval,sock,recvmsg(sock,message,flags));
  VALUES1(fixnum(retval)); skipSTACK(2);
}

DEFUN(RAWSOCK:SOCK-READ,socket buffer) {
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  void *buffer;
  size_t buffer_len;
  STACK_0 = check_buffer_arg(STACK_0);
  buffer = (void*)TheSbvector(STACK_1)->data;
  buffer_len = Sbvector_length(STACK_1);
  SYSCALL(retval,sock,read(sock,buffer,buffer_len));
  VALUES1(fixnum(retval)); skipSTACK(2);
}

/* ================== SENDING ================== */

/* remove 2 objects from the STACK and return the SEND flag
   based on MSG_OOB MSG_EOR */
int send_flags (void) {
   int flags = 0
#  if defined(MSG_EOR)
    | (missingp(STACK_0) ? 0 : MSG_EOR)
#  endif
#  if defined(MSG_OOB)
    | (missingp(STACK_1) ? 0 : MSG_OOB)
#  endif
    ;
   skipSTACK(2);
   return flags;
}

DEFUN(RAWSOCK:SEND,socket buffer &key MSG_OOB MSG_EOR) {
  int flags = send_flags();
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  void *buffer;
  size_t buffer_len;
  STACK_0 = check_buffer_arg(STACK_0);
  buffer = (void*)TheSbvector(STACK_1)->data;
  buffer_len = Sbvector_length(STACK_1);
  SYSCALL(retval,sock,send(sock,buffer,buffer_len,flags));
  VALUES1(fixnum(retval)); skipSTACK(2);
}

DEFUN(RAWSOCK:SENDMSG,socket message &key MSG_OOB MSG_EOR) {
  int flags = send_flags();
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  size_t size;
  struct msghdr *message =
    (struct msghdr*)check_struct_data(`RAWSOCK::MSGHDR`,STACK_0,&size);
  SYSCALL(retval,sock,sendmsg(sock,message,flags));
  VALUES1(fixnum(retval)); skipSTACK(2);
}

DEFUN(RAWSOCK:SENDTO, socket buffer address &key MSG_OOB MSG_EOR) {
  int flags = send_flags();
  int sock = posfixnum_to_L(check_posfixnum(STACK_2)), retval;
  struct sockaddr *sa;
  void *buffer;
  size_t buffer_len, size;
  STACK_1 = check_buffer_arg(STACK_1);
  sa = (struct sockaddr*)check_struct_data(`RAWSOCK::SOCKADDR`,STACK_0,&size);
  /* no GC after this point! */
  buffer = (void*)TheSbvector(STACK_1)->data;
  buffer_len = Sbvector_length(STACK_1);
  SYSCALL(retval,sock,
          sendto(sock,buffer,buffer_len,flags,sa,sizeof(struct sockaddr)));
  VALUES1(fixnum(retval)); skipSTACK(2);
}

DEFUN(RAWSOCK:SOCK-WRITE,socket buffer) {
  int sock = posfixnum_to_L(check_posfixnum(STACK_1)), retval;
  void *buffer;
  size_t buffer_len;
  STACK_0 = check_buffer_arg(STACK_0);
  buffer = (void*)TheSbvector(STACK_1)->data;
  buffer_len = Sbvector_length(STACK_1);
  SYSCALL(retval,sock,write(sock,buffer,buffer_len));
  VALUES1(fixnum(retval)); skipSTACK(2);
}

DEFUN(RAWSOCK:CLOSESOCK, socket) {
  int sock = posfixnum_to_L(check_posfixnum(popSTACK())), retval;
  SYSCALL(retval,sock,close(sock));
  VALUES1(fixnum(retval));
}

DEFUN(RAWSOCK:SHUTDOWN, socket direction) {
  direction_t direction = check_direction(popSTACK());
  int sock = posfixnum_to_L(check_posfixnum(popSTACK()));
  int how, retval;
  switch (direction) {
    case DIRECTION_PROBE: case DIRECTION_IO: how = SHUT_RDWR; break;
    case DIRECTION_INPUT_IMMUTABLE: case DIRECTION_INPUT: how = SHUT_RD; break;
    case DIRECTION_OUTPUT: how = SHUT_WR; break;
    default: NOTREACHED;
  }
  SYSCALL(retval,sock,shutdown(sock,how));
  VALUES1(fixnum(retval));
}

/* STACK_3 = name, for error reporting */
static void configdev (int sock, char* name, int ipaddress, int flags) {
  struct ifreq ifrequest;
#if defined(SIOCGIFFLAGS) && defined(SIOCSIFFLAGS)
  memset(&ifrequest, 0, sizeof(struct ifreq));
  strcpy(ifrequest.ifr_name, name);
  if (ioctl(sock, SIOCGIFFLAGS, &ifrequest) < 0)
    OS_file_error(STACK_3);
  ifrequest.ifr_flags |= flags;
  if (ioctl(sock, SIOCSIFFLAGS, &ifrequest) < 0)
    OS_file_error(STACK_3);
#endif
#if defined(SIOCGIFADDR) && defined(SIOCSIFADDR)
  memset(&ifrequest, 0, sizeof(struct ifreq));
  strcpy(ifrequest.ifr_name, name);
  if (ioctl(sock, SIOCGIFADDR, &ifrequest) < 0)
    OS_file_error(STACK_3);
  /* address was 0.0.0.0 -> error */
  if (ipaddress != 0) {
    if (ioctl(sock, SIOCGIFADDR, &ifrequest) < 0)
      OS_file_error(STACK_3);
    else {
      register int j;
      for (j=2;j<6;j++) ifrequest.ifr_addr.sa_data[j] = 0;
      if (ioctl(sock, SIOCSIFADDR,  &ifrequest) < 0)
        OS_file_error(STACK_3);
    }
  }
#endif
}

DEFUN(RAWSOCK:CONFIGDEV, socket name ipaddress &key PROMISC NOARP) {
  int flags = 0
#  if defined(IFF_NOARP)
    | (missingp(STACK_0) ? 0 : IFF_NOARP)
#  endif
#  if defined(IFF_PROMISC)
    | (missingp(STACK_1) ? 0 : IFF_PROMISC)
#  endif
    ;
  uint32_t ipaddress = I_to_UL(check_uint32(STACK_2));
  int sock = posfixnum_to_L(check_posfixnum(STACK_4));
  with_string_0(check_string(STACK_3),Symbol_value(S(utf_8)),name, {
      begin_system_call();
      configdev(sock, name, ipaddress, flags);
      end_system_call();
    });
  VALUES0;
}

/* ================== CHECKSUM from Fred Cohen ================== */

DEFUN(RAWSOCK:IPCSUM, &optional buffer) { /* IP CHECKSUM */
  unsigned char* buffer = TheSbvector(check_buffer_arg(popSTACK()))->data;
  register long sum=0;           /* assumes long == 32 bits */
  unsigned short result;
  unsigned char *ptr=&(buffer[14]);
  unsigned int nbytes;
  buffer[24]=0;buffer[25]=0;nbytes=(buffer[14] & 0xF) << 2; /* checksum=0, headerlen */
  while(nbytes>1){sum += *ptr; ptr++; sum += *ptr <<8; ptr++; nbytes -= 2;}
  if(nbytes==1){sum += *ptr;}     /* mop up an odd byte,  if necessary */
  sum = (sum >> 16) + (sum & 0xFFFF);
  result=~(sum  + (sum >> 16)) & 0xFFFF;
  buffer[24]=(result & 0xFF);
  buffer[25]=((result >> 8) & 0xFF);
  VALUES1(fixnum(result));
}

DEFUN(RAWSOCK:ICMPCSUM, &optional buffer) { /* ICMP CHECKSUM */
  unsigned char* buffer = TheSbvector(check_buffer_arg(popSTACK()))->data;
  register long sum=0;           /* assumes long == 32 bits */
  unsigned short result;
  unsigned char *ptr;
  unsigned int nbytes, off, offset;
  off=((buffer[14]&0xF)<<2);offset=off+14; /* start of ICMP header */
  buffer[offset+2]=0;buffer[offset+3]=0;
  nbytes=(((buffer[16])<<8)+(buffer[17]))-off; /* bytes in ICMP part */
  ptr=&(buffer[offset]);
  while(nbytes>1){sum += *ptr; ptr++; sum += *ptr <<8; ptr++; nbytes -= 2;}
  if(nbytes==1){sum += *ptr;}     /* mop up an odd byte,  if necessary */
  sum = (sum >> 16) + (sum & 0xFFFF);
  result=~(sum  + (sum >> 16)) & 0xFFFF;
  buffer[offset+2]=(result & 0xFF);
  buffer[offset+3]=((result >> 8) & 0xFF);
  VALUES1(fixnum(result));
}

DEFUN(RAWSOCK:TCPCSUM, &optional buffer) {      /* TCP checksum */
  unsigned char* buffer = TheSbvector(check_buffer_arg(popSTACK()))->data;
  register unsigned long sum;  /* assumes long == 32 bits */
  unsigned short result;
  unsigned char *ptr;
  unsigned int nbytes, packsize, offset;
  sum = (buffer[26]<<8)+ buffer[27]+(buffer[28]<<8)+ buffer[29];  /* Src IP */
  sum +=(buffer[30]<<8)+ buffer[31]+(buffer[32]<<8)+ buffer[33];  /* Dst IP */
  sum +=(buffer[23]);           /* zero followed by protocol */
  packsize=((buffer[16])<<8)+(buffer[17]); /* packet size - not including ARP area */
  offset=((buffer[14]&0xF)<<2); /* start of TCP header (rel to IP header) */
  sum +=(packsize - offset);    /* size of TCP part of the packet */
  ptr=&(buffer[offset+14]);     /* start of TCP header in buffer */
  nbytes=packsize-offset;       /* number of bytes to checksum */
  buffer[offset+16+14]=0;
  buffer[offset+17+14]=0; /* initialize TCP checksum to 0 */
  while(nbytes>1){sum += *ptr<<8; ptr++; sum += *ptr; ptr++; nbytes -= 2;}
  if (nbytes==1) {sum += *ptr<<8;} /* mop up an odd byte,  if necessary */
  sum = (sum >> 16) + (sum & 0xFFFF);
  result=~(sum  + (sum >> 16)) & 0xFFFF;
  buffer[offset+17+14]=(result & 0xFF);
  buffer[offset+16+14]=((result >> 8) & 0xFF);
  VALUES1(fixnum(result));
}

DEFUN(RAWSOCK:UDPCSUM, &optional buffer) { /* UDP checksum */
  unsigned char* buffer = TheSbvector(check_buffer_arg(popSTACK()))->data;
  register unsigned long sum = 0;  /* assumes long == 32 bits */
  unsigned short result;
  unsigned char *ptr;
  unsigned int nbytes, packsize, offset;
  sum = (buffer[26]<<8)+ buffer[27]+(buffer[28]<<8)+ buffer[29];  /* Src IP */
  sum +=(buffer[30]<<8)+ buffer[31]+(buffer[32]<<8)+ buffer[33];  /* Dst IP */
  sum +=(buffer[23]);           /* zero followed by protocol */
  packsize=((buffer[16])<<8)+(buffer[17]); /* packet size */
  offset=((buffer[14]&0xF)<<2);            /* start of UDP header */
  sum +=(((buffer[16])<<8)+(buffer[17])) -offset;
  ptr=&(buffer[offset+14]);     /* start of TCP header */
  nbytes=packsize-offset;
  buffer[offset+6+14]=0;
  buffer[offset+7+14]=0; /* initialize UDP checksum to 0 */
  while(nbytes>1){sum += *ptr <<8; ptr++; sum += *ptr; ptr++; nbytes -= 2;}
  if (nbytes==1) {sum += *ptr<<8;} /* mop up an odd byte, if necessary */
  sum = (sum >> 16) + (sum & 0xFFFF);
  result=~(sum  + (sum >> 16)) & 0xFFFF;
  buffer[offset+7+14]=(result & 0xFF);
  buffer[offset+6+14]=((result >> 8) & 0xFF);
  VALUES1(fixnum(result));
}
