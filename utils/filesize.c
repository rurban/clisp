/*
 * Replace the magic marker in the file with the file size
 * Sam Steingold 2006
 * Released under the GNU GPL v2
 */

#include <stdio.h>
#include <string.h>

static long find_marker (FILE* file, char *marker) {
  char buf[BUFSIZ];
  size_t marker_len = strlen(marker);
  size_t marker_pos = 0;
  long pos = 0;
  while (1) {
    size_t result = fread(buf,sizeof(char),BUFSIZ,file);
    size_t i;
    if (result <= 0)
      return -1L;
    for (i = 0; i < result; i++) {
      pos++;
      if (buf[i] == marker[marker_pos]) {
        if (++marker_pos == marker_len) /* found! */
          return pos - marker_len;
      } else
        marker_pos = 0;
    }
  }
  return -1L;
}

int main (int argc, char *argv[]) {
  FILE *file;
  char buf[BUFSIZ];
  char marker[16] = "my magic marker";
  size_t marker_len = strlen(marker);
  long size, pos;
  size_t res;
  if (argc != 2) {              /* check arguments */
    fprintf(stderr,"Usage: %s filename\nTo replace '%s' with file size\n",
            argv[0],marker);
    return 1;
  }
  if ((file = fopen(argv[1],"rb+")) == NULL) { /* OPEN */
    sprintf(buf,"%s: cannot open '%s'",argv[0],argv[1]);
    perror(buf);
    return 2;
  }
  printf("%s: opened '%s'\n",argv[0],argv[1]);
  /* find out the file size */
  if (fseek(file,0,SEEK_END)) {
    sprintf(buf,"%s: cannot fseek '%s' to END",argv[0],argv[1]);
    perror(buf);
    fclose(file);
    return 3;
  }
  if ((size = ftell(file)) == -1L) {
    sprintf(buf,"%s: cannot ftell '%s'",argv[0],argv[1]);
    perror(buf);
    fclose(file);
    return 4;
  }
  printf("%s: size of '%s' is %ld\n",argv[0],argv[1],size);
  /* rewind to start */
  if (fseek(file,0,SEEK_SET)) {
    sprintf(buf,"%s: cannot fseek '%s' to SET",argv[0],argv[1]);
    perror(buf);
    fclose(file);
    return 5;
  }
  /* find the marker */
  if ((pos = find_marker(file,marker)) == -1L) {
    fprintf(stderr,"%s: cannot find '%s' in '%s'",argv[0],marker,argv[1]);
    fclose(file);
    return 6;
  }
  printf("%s: found '%s' in '%s' at %ld\n",argv[0],marker,argv[1],pos);
  /* move to the marker */
  if (fseek(file,pos,SEEK_SET)) {
    sprintf(buf,"%s: cannot fseek '%s' to SET %ld",argv[0],argv[1],pos);
    perror(buf);
    fclose(file);
    return 7;
  }
  /* make sure that we did indeed find the marker */
  if ((res = fread(buf,sizeof(char),BUFSIZ,file)) <= marker_len) {
    fprintf(stderr,"%s: fread returns %d (%s)\n",argv[0],res,buf);
    fclose(file);
    return 8;
  }
  if (strncmp(marker,buf,marker_len)) {
    fprintf(stderr,"%s: marker mismatch: %s\n",argv[0],buf);
    fclose(file);
    return 8;
  }
  /* move to the marker */
  if (fseek(file,pos,SEEK_SET)) {
    sprintf(buf,"%s: cannot fseek '%s' to SET %ld",argv[0],argv[1],pos);
    perror(buf);
    fclose(file);
    return 9;
  }
  /* rewrite the marker */
  for (res = 0; res < marker_len; res++) buf[res] = 0;
  sprintf(buf,"%ld",size);
  if (fwrite(buf,sizeof(char),marker_len,file) <= 0) {
    char buf1[BUFSIZ];
    sprintf(buf1,"%s: cannot fwrite '%s' to '%s'",argv[0],buf,argv[1]);
    perror(buf1);
    fclose(file);
    return 10;
  }
  fclose(file);
  return 0;
}
