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
#include <sys/ioctl.h>
#endif
#if defined(HAVE_ERRNO_H)
# include <errno.h>
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
void* check_struct_data (object type, object arg) {
  object vec = TheStructure(check_struct(type,arg))->recdata[1];
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
  struct sockaddr *sa =
    (struct sockaddr*)check_struct_data(`RAWSOCK:SOCKADDR`,popSTACK());
  VALUES1(fixnum(sa->sa_family));
}

/* invoke system call C, place return value in R, report error on socket C */
#define SYSCALL(r,s,c)                                  \
  do { begin_system_call(); r = c; end_system_call();   \
    if (r<0) {                                          \
      if (s<=0) OS_error();                             \
      else OS_filestream_error(fixnum(s));              \
    }                                                   \
  } while(0)


/* ================== sys/socket.h interface ================== */

DEFUN(RAWSOCK:SOCKET,domain type protocol) {
  int sock, domain, type, protocol;
 restart_check_protocol:        /* PROTOCOL */
  if (!posfixnump(STACK_0)) {
    if (nullp(STACK_0)) protocol = 0;
#  if defined(ETH_P_LOOP)
    else if (eq(STACK_0,`:ETH_P_LOOP`))      protocol = ETH_P_LOOP;
#  endif
#  if defined(ETH_P_PUP)
    else if (eq(STACK_0,`:ETH_P_PUP`))       protocol = ETH_P_PUP;
#  endif
#  if defined(ETH_P_PUPAT)
    else if (eq(STACK_0,`:ETH_P_PUPAT`))     protocol = ETH_P_PUPAT;
#  endif
#  if defined(ETH_P_IP)
    else if (eq(STACK_0,`:ETH_P_IP`))        protocol = ETH_P_IP;
#  endif
#  if defined(ETH_P_X25)
    else if (eq(STACK_0,`:ETH_P_X25`))       protocol = ETH_P_X25;
#  endif
#  if defined(ETH_P_ARP)
    else if (eq(STACK_0,`:ETH_P_ARP`))       protocol = ETH_P_ARP;
#  endif
#  if defined(ETH_P_BPQ)
    else if (eq(STACK_0,`:ETH_P_BPQ`))       protocol = ETH_P_BPQ;
#  endif
#  if defined(ETH_P_IEEEPUP)
    else if (eq(STACK_0,`:ETH_P_IEEEPUP`))   protocol = ETH_P_IEEEPUP;
#  endif
#  if defined(ETH_P_IEEEPUPAT)
    else if (eq(STACK_0,`:ETH_P_IEEEPUPAT`)) protocol = ETH_P_IEEEPUPAT;
#  endif
#  if defined(ETH_P_DEC)
    else if (eq(STACK_0,`:ETH_P_DEC`))       protocol = ETH_P_DEC;
#  endif
#  if defined(ETH_P_DNA_DL)
    else if (eq(STACK_0,`:ETH_P_DNA_DL`))    protocol = ETH_P_DNA_DL;
#  endif
#  if defined(ETH_P_DNA_RC)
    else if (eq(STACK_0,`:ETH_P_DNA_RC`))    protocol = ETH_P_DNA_RC;
#  endif
#  if defined(ETH_P_DNA_RT)
    else if (eq(STACK_0,`:ETH_P_DNA_RT`))    protocol = ETH_P_DNA_RT;
#  endif
#  if defined(ETH_P_LAT)
    else if (eq(STACK_0,`:ETH_P_LAT`))       protocol = ETH_P_LAT;
#  endif
#  if defined(ETH_P_DIAG)
    else if (eq(STACK_0,`:ETH_P_DIAG`))      protocol = ETH_P_DIAG;
#  endif
#  if defined(ETH_P_CUST)
    else if (eq(STACK_0,`:ETH_P_CUST`))      protocol = ETH_P_CUST;
#  endif
#  if defined(ETH_P_SCA)
    else if (eq(STACK_0,`:ETH_P_SCA`))       protocol = ETH_P_SCA;
#  endif
#  if defined(ETH_P_RARP)
    else if (eq(STACK_0,`:ETH_P_RARP`))      protocol = ETH_P_RARP;
#  endif
#  if defined(ETH_P_ATALK)
    else if (eq(STACK_0,`:ETH_P_ATALK`))     protocol = ETH_P_ATALK;
#  endif
#  if defined(ETH_P_AARP)
    else if (eq(STACK_0,`:ETH_P_AARP`))      protocol = ETH_P_AARP;
#  endif
#  if defined(ETH_P_IPX)
    else if (eq(STACK_0,`:ETH_P_IPX`))       protocol = ETH_P_IPX;
#  endif
#  if defined(ETH_P_IPV6)
    else if (eq(STACK_0,`:ETH_P_IPV6`))      protocol = ETH_P_IPV6;
#  endif
#  if defined(ETH_P_PPP_DISC)
    else if (eq(STACK_0,`:ETH_P_PPP_DISC`))  protocol = ETH_P_PPP_DISC;
#  endif
#  if defined(ETH_P_PPP_SES)
    else if (eq(STACK_0,`:ETH_P_PPP_SES`))   protocol = ETH_P_PPP_SES;
#  endif
#  if defined(ETH_P_ATMMPOA)
    else if (eq(STACK_0,`:ETH_P_ATMMPOA`))   protocol = ETH_P_ATMMPOA;
#  endif
#  if defined(ETH_P_ATMFATE)
    else if (eq(STACK_0,`:ETH_P_ATMFATE`))   protocol = ETH_P_ATMFATE;
#  endif
#  if defined(ETH_P_802_3)
    else if (eq(STACK_0,`:ETH_P_802_3`))     protocol = ETH_P_802_3;
#  endif
#  if defined(ETH_P_AX25)
    else if (eq(STACK_0,`:ETH_P_AX25`))      protocol = ETH_P_AX25;
#  endif
#  if defined(ETH_P_ALL)
    else if (eq(STACK_0,`:ETH_P_ALL`))       protocol = ETH_P_ALL;
#  endif
#  if defined(ETH_P_802_2)
    else if (eq(STACK_0,`:ETH_P_802_2`))     protocol = ETH_P_802_2;
#  endif
#  if defined(ETH_P_SNAP)
    else if (eq(STACK_0,`:ETH_P_SNAP`))      protocol = ETH_P_SNAP;
#  endif
#  if defined(ETH_P_DDCMP)
    else if (eq(STACK_0,`:ETH_P_DDCMP`))     protocol = ETH_P_DDCMP;
#  endif
#  if defined(ETH_P_WAN_PPP)
    else if (eq(STACK_0,`:ETH_P_WAN_PPP`))   protocol = ETH_P_WAN_PPP;
#  endif
#  if defined(ETH_P_PPP_MP)
    else if (eq(STACK_0,`:ETH_P_PPP_MP`))    protocol = ETH_P_PPP_MP;
#  endif
#  if defined(ETH_P_LOCALTALK)
    else if (eq(STACK_0,`:ETH_P_LOCALTALK`)) protocol = ETH_P_LOCALTALK;
#  endif
#  if defined(ETH_P_PPPTALK)
    else if (eq(STACK_0,`:ETH_P_PPPTALK`))   protocol = ETH_P_PPPTALK;
#  endif
#  if defined(ETH_P_TR_802_2)
    else if (eq(STACK_0,`:ETH_P_TR_802_2`))  protocol = ETH_P_TR_802_2;
#  endif
#  if defined(ETH_P_MOBITEX)
    else if (eq(STACK_0,`:ETH_P_MOBITEX`))   protocol = ETH_P_MOBITEX;
#  endif
#  if defined(ETH_P_CONTROL)
    else if (eq(STACK_0,`:ETH_P_CONTROL`))   protocol = ETH_P_CONTROL;
#  endif
#  if defined(ETH_P_IRDA)
    else if (eq(STACK_0,`:ETH_P_IRDA`))      protocol = ETH_P_IRDA;
#  endif
#  if defined(ETH_P_ECONET)
    else if (eq(STACK_0,`:ETH_P_ECONET`))    protocol = ETH_P_ECONET;
#  endif
    else {
      STACK_0 = my_check_argument(`PROTOCOL`,STACK_0);
      goto restart_check_protocol;
    }
  } else protocol = posfixnum_to_L(STACK_0);
  skipSTACK(1);                 /* drop PROTOCOL argument */
 restart_check_type:            /* TYPE */
  if (!posfixnump(STACK_0)) {
    if (nullp(STACK_0)) type = 0;
#  if defined(SOCK_STREAM)
    else if (eq(STACK_0,`:SOCK_STREAM`))    type = SOCK_STREAM;
#  endif
#  if defined(SOCK_DGRAM)
    else if (eq(STACK_0,`:SOCK_DGRAM`))     type = SOCK_DGRAM;
#  endif
#  if defined(SOCK_RAW)
    else if (eq(STACK_0,`:SOCK_RAW`))       type = SOCK_RAW;
#  endif
#  if defined(SOCK_RDM)
    else if (eq(STACK_0,`:SOCK_RDM`))       type = SOCK_RDM;
#  endif
#  if defined(SOCK_SEQPACKET)
    else if (eq(STACK_0,`:SOCK_SEQPACKET`)) type = SOCK_SEQPACKET;
#  endif
#  if defined(SOCK_PACKET)
    else if (eq(STACK_0,`:SOCK_PACKET`))    type = SOCK_PACKET;
#  endif
    else {
      my_check_argument(`TYPE`,STACK_0);
      goto restart_check_type;
    }
  }
  else type = posfixnum_to_L(STACK_0);
  skipSTACK(1);                 /* drop TYPE argument */
 restart_check_domain:          /* DOMAIN */
  if (!posfixnump(STACK_0)) {
    if (nullp(STACK_0)) domain = 0;
#  if defined(AF_UNSPEC)
    else if (eq(STACK_0,`:AF_UNSPEC`))    domain = AF_UNSPEC;
#  endif
#  if defined(AF_UNIX)
    else if (eq(STACK_0,`:AF_UNIX`))      domain = AF_UNIX;
#  endif
#  if defined(AF_LOCAL)
    else if (eq(STACK_0,`:AF_LOCAL`))     domain = AF_LOCAL;
#  endif
#  if defined(AF_INET)
    else if (eq(STACK_0,`:AF_INET`))      domain = AF_INET;
#  endif
#  if defined(AF_AX25)
    else if (eq(STACK_0,`:AF_AX25`))      domain = AF_AX25;
#  endif
#  if defined(AF_IPX)
    else if (eq(STACK_0,`:AF_IPX`))       domain = AF_IPX;
#  endif
#  if defined(AF_APPLETALK)
    else if (eq(STACK_0,`:AF_APPLETALK`)) domain = AF_APPLETALK;
#  endif
#  if defined(AF_NETROM)
    else if (eq(STACK_0,`:AF_NETROM`))    domain = AF_NETROM;
#  endif
#  if defined(AF_BRIDGE)
    else if (eq(STACK_0,`:AF_BRIDGE`))    domain = AF_BRIDGE;
#  endif
#  if defined(AF_ATMPVC)
    else if (eq(STACK_0,`:AF_ATMPVC`))    domain = AF_ATMPVC;
#  endif
#  if defined(AF_X25)
    else if (eq(STACK_0,`:AF_X25`))       domain = AF_X25;
#  endif
#  if defined(AF_INET6)
    else if (eq(STACK_0,`:AF_INET6`))     domain = AF_INET6;
#  endif
#  if defined(AF_ROSE)
    else if (eq(STACK_0,`:AF_ROSE`))      domain = AF_ROSE;
#  endif
#  if defined(AF_DECnet)
    else if (eq(STACK_0,`:AF_DECnet`))    domain = AF_DECnet;
#  endif
#  if defined(AF_NETBEUI)
    else if (eq(STACK_0,`:AF_NETBEUI`))   domain = AF_NETBEUI;
#  endif
#  if defined(AF_SECURITY)
    else if (eq(STACK_0,`:AF_SECURITY`))  domain = AF_SECURITY;
#  endif
#  if defined(AF_KEY)
    else if (eq(STACK_0,`:AF_KEY`))       domain = AF_KEY;
#  endif
#  if defined(AF_NETLINK)
    else if (eq(STACK_0,`:AF_NETLINK`))   domain = AF_NETLINK;
#  endif
#  if defined(AF_ROUTE)
    else if (eq(STACK_0,`:AF_ROUTE`))     domain = AF_ROUTE;
#  endif
#  if defined(AF_PACKET)
    else if (eq(STACK_0,`:AF_PACKET`))    domain = AF_PACKET;
#  endif
#  if defined(AF_ASH)
    else if (eq(STACK_0,`:AF_ASH`))       domain = AF_ASH;
#  endif
#  if defined(AF_ECONET)
    else if (eq(STACK_0,`:AF_ECONET`))    domain = AF_ECONET;
#  endif
#  if defined(AF_ATMSVC)
    else if (eq(STACK_0,`:AF_ATMSVC`))    domain = AF_ATMSVC;
#  endif
#  if defined(AF_SNA)
    else if (eq(STACK_0,`:AF_SNA`))       domain = AF_SNA;
#  endif
#  if defined(AF_IRDA)
    else if (eq(STACK_0,`:AF_IRDA`))      domain = AF_IRDA;
#  endif
#  if defined(AF_PPPOX)
    else if (eq(STACK_0,`:AF_PPPOX`))     domain = AF_PPPOX;
#  endif
#  if defined(AF_WANPIPE)
    else if (eq(STACK_0,`:AF_WANPIPE`))   domain = AF_WANPIPE;
#  endif
#  if defined(AF_BLUETOOTH)
    else if (eq(STACK_0,`:AF_BLUETOOTH`)) domain = AF_BLUETOOTH;
#  endif
    else {
      my_check_argument(`DOMAIN`,STACK_0);
      goto restart_check_domain;
    }
  } else type = posfixnum_to_L(STACK_0);
  skipSTACK(1);                 /* drop DOMAIN argument */
  SYSCALL(sock,-1,socket(domain, type, protocol));
  VALUES1(fixnum(sock));
} /* RAWSOCK:SOCKET */

DEFUN(RAWSOCK:CLOSESOCK, socket) {
  int sock = posfixnum_to_L(check_posfixnum(popSTACK())), retval;
  SYSCALL(retval,sock,close(sock));
  VALUES1(fixnum(retval));
}

DEFUN(RAWSOCK:RECVFROM, socket buffer address \
      &key MSG_PEEK MSG_OOB MSG_WAITALL) {
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
  int sock, retval;
  struct sockaddr *sa = NULL;
  void *buffer;
  size_t buffer_len, sa_size = sizeof(struct sockaddr);
  skipSTACK(3);                 /* drop flags */
  STACK_1 = check_buffer_arg(STACK_1);
  if (nullp(STACK_0)) {
    pushSTACK(allocate_bit_vector(Atype_8Bit,sa_size));
    funcall(`RAWSOCK:MAKE-SOCKADDR`,1);
    STACK_0 = value1;
  }
  sa = (struct sockaddr*)check_struct_data(`RAWSOCK:SOCKADDR`,STACK_0);
  /* no GC after this point! */
  buffer = (void*)TheSbvector(STACK_1)->data;
  buffer_len = vector_length(STACK_1);
  sock = posfixnum_to_L(check_posfixnum(STACK_2));
  SYSCALL(retval,sock,recvfrom(sock,buffer,buffer_len,flags,sa,&sa_size));
  VALUES3(fixnum(retval),fixnum(sa_size),STACK_0);
  skipSTACK(3);
}

DEFUN(RAWSOCK:SENDTO, socket buffer address &key MSG_OOB MSG_EOR) {
  int flags = 0
#  if defined(MSG_EOR)
    | (missingp(STACK_0) ? 0 : MSG_EOR)
#  endif
#  if defined(MSG_OOB)
    | (missingp(STACK_1) ? 0 : MSG_OOB)
#  endif
    ;
  int sock, retval;
  struct sockaddr *sa;
  void *buffer;
  size_t buffer_len;
  skipSTACK(2);                 /* drop flags */
  STACK_1 = check_buffer_arg(STACK_1);
  sa = (struct sockaddr*)check_struct_data(`RAWSOCK:SOCKADDR`,STACK_0);
  /* no GC after this point! */
  buffer = (void*)TheSbvector(STACK_1)->data;
  buffer_len = vector_length(STACK_1);
  sock = posfixnum_to_L(check_posfixnum(STACK_2));
  SYSCALL(retval,sock,
          sendto(sock,buffer,buffer_len,flags,sa,sizeof(struct sockaddr)));
  skipSTACK(2);
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
