/*
 * Module for Raw Sockets / CLISP
 * Fred Cohen, 2003-2004
 * Don Cohen, 2003-2004
 * Sam Steingold 2004
 * <http://www.opengroup.org/onlinepubs/007908799/xns/syssocket.h.html>
 */

#include "clisp.h"

#if defined(HAVE_SYS_TYPES_H)
# include <sys/types.h>
#endif
#if defined(STDC_HEADERS)
# include <stdio.h>
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

#define snaplen 1518
#define MAX_DEVICE_NAME_SIZE 14
#define BUFSIZE 2*snaplen
#define ETH_HEAD_LEN    14
#define ARP_HEAD_LEN    28
#define IP_HEAD_LEN     20    /* don't use ----ip varible size use ip->ip_hl = num of 32 bit words */
#define TCP_HEAD_LEN    20    /* don't use ----ip varible size use ip->ip_hl = num of 32 bit words */
#define TCP_HEAD_BASE   (ETH_HEAD_LEN + IP_HEAD_LEN)
#define FIN  0x01
#define SYN  0x02
#define RST  0x04
#define PSH  0x08
#define ACK  0x10
#define URG  0x20

#if defined(HAVE_ERRNO_H)
# include <errno.h>
#endif

DEFMODULE(rawsock,"RAWSOCK")

nonreturning_function(static,my_type_error,(object type, object datum)) {
  pushSTACK(datum);             /* TYPE-ERROR slot DATUM */
  pushSTACK(type);              /* TYPE-ERROR slot TYPE */
  pushSTACK(type); pushSTACK(datum); pushSTACK(TheSubr(subr_self)->name);
  fehler (type_error, ("~S: ~S is not of type ~S"));
}
static unsigned char* check_buffer_arg (object arg) {
  if (missingp(arg)) {
    return TheSbvector(O(buffer))->data;
  } else if (simple_bit_vector_p(Atype_8Bit,arg)) {
    return TheSbvector(arg)->data;
  } else
    my_type_error(arg,S(simple_bit_vector));
}

DEFVAR(buffer,`#.(MAKE-ARRAY 1518 :ELEMENT-TYPE (QUOTE (UNSIGNED-BYTE 8)))`);

DEFUN(RAWSOCK:BUFFER,) { VALUES1(O(buffer)); }

DEFUN(RAWSOCK:SOCKET,) {        /* domain type protocol */
  begin_system_call();
  VALUES1(fixnum(socket(PF_INET, SOCK_PACKET, htons(ETH_P_ALL))));
  end_system_call();
}

DEFUN(RAWSOCK:CLOSESOCK, socket) {
  int socket = posfixnum_to_L(check_posfixnum(popSTACK()));
  begin_system_call();
  VALUES1(fixnum(close(socket)));
  end_system_call();
}

DEFUN(RAWSOCK:RECVFROM, socket) {
  int socket = posfixnum_to_L(check_posfixnum(popSTACK()));
  begin_system_call();
  VALUES1(fixnum(recvfrom(socket, TheSbvector(O(buffer))->data,
                          vector_length(O(buffer)), 0, NULL, 0)));
  end_system_call();
}

DEFUN(RAWSOCK:SENDTO, socket &key SIZE) {
  int size = (missingp(STACK_0) ? skipSTACK(1), vector_length(O(buffer))
              : posfixnum_to_L(check_posfixnum(popSTACK())));
  int socket = posfixnum_to_L(check_posfixnum(popSTACK()));
  begin_system_call();
  VALUES1(fixnum(sendto(socket, TheSbvector(O(buffer))->data,
                        size, 0, NULL, 0)));
  end_system_call();
}

DEFUN(RAWSOCK:SHUTDOWN, socket direction) {
  direction_t direction = check_direction(popSTACK());
  int socket = posfixnum_to_L(check_posfixnum(popSTACK()));
  int how;
  switch (direction) {
    case DIRECTION_PROBE: case DIRECTION_IO: how = SHUT_RDWR; break;
    case DIRECTION_INPUT_IMMUTABLE: case DIRECTION_INPUT: how = SHUT_RD; break;
    case DIRECTION_OUTPUT: how = SHUT_WR; break;
    default: NOTREACHED;
  }
  begin_system_call();
  VALUES1(fixnum(shutdown(socket,how)));
  end_system_call();
}

/* STACK_3 = name, for error reporting */
local void configdev (int socket, char* name, int ipaddress, int flags) {
  struct ifreq ifrequest;
  memset(&ifrequest, 0, sizeof(struct ifreq));
  strcpy(ifrequest.ifr_name, name);
  if (ioctl(socket, SIOCGIFFLAGS, &ifrequest) < 0)
    OS_file_error(STACK_3);
  ifrequest.ifr_flags |= flags;
  if (ioctl(socket, SIOCSIFFLAGS, &ifrequest) < 0)
    OS_file_error(STACK_3);
  memset(&ifrequest, 0, sizeof(struct ifreq));
  strcpy(ifrequest.ifr_name, name);
  if (ioctl(socket, SIOCGIFADDR, &ifrequest) < 0)
    OS_file_error(STACK_3);
  /* add was 0.0.0.0 -> error */
  if (ipaddress != 0) {
    if (ioctl(socket, SIOCGIFADDR, &ifrequest) < 0)
      OS_file_error(STACK_3);
    else {
      register int j;
      for (j=2;j<6;j++) ifrequest.ifr_addr.sa_data[j] = 0;
      if (ioctl(socket, SIOCSIFADDR,  &ifrequest) < 0)
        OS_file_error(STACK_3);
    }
  }
}

DEFUN(RAWSOCK:CONFIGDEV, socket name ipaddress &key PROMISC NOARP) {
  int flags = 0
    | (missingp(STACK_0) ? 0 : IFF_NOARP)
    | (missingp(STACK_1) ? 0 : IFF_PROMISC);
  uint32_t ipaddress = I_to_UL(check_uint32(STACK_2));
  int socket = posfixnum_to_L(check_posfixnum(STACK_4));
  with_string_0(check_string(STACK_3),Symbol_value(S(utf_8)),name, {
      begin_system_call();
      configdev(socket, name, ipaddress, flags);
      end_system_call();
    });
  VALUES0;
}

DEFUN(RAWSOCK:IPCSUM, &optional buffer) { /* IP CHECKSUM */
  unsigned char* buffer = check_buffer_arg(popSTACK());
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
  unsigned char* buffer = check_buffer_arg(popSTACK());
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
  unsigned char* buffer = check_buffer_arg(popSTACK());
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
  unsigned char buffer = check_buffer_arg(popSTACK());
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
