#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
/* Only defined on Windows, etc */
#ifndef O_BINARY
#define  O_BINARY 0
#endif

#ifndef OPEN_MAX
/* Solaris' value: not set on Windows. Just needs to be large enough */
#  define OPEN_MAX 64
#endif

#include "R.h"


static char *stream_names[OPEN_MAX];
static int   stream_modes[OPEN_MAX];


static void swap(void *result, int size)
{
    int i;
    char *p = result, tmp;

    if (size == 1) return;
    for (i = 0; i < size/2; i++) {
	tmp = p[i];
	p[i] = p[size - i - 1];
	p[size - i - 1] = tmp;
    }
}


/* mode is 1 for read, 2 for write */
void openstream(char **filename, int *mode, int *handle)
{
    int fd;
    char *p = R_ExpandFileName(filename[0]);

    if(*mode == 1)
	fd = open(p, O_RDONLY|O_BINARY);
    else
	fd = open(p, O_WRONLY|O_CREAT|O_TRUNC|O_BINARY, S_IRUSR|S_IWUSR);
    if (fd == -1) error("opening `%s' failed", p);
    *handle = fd;
    stream_modes[fd] = 100 + (*mode);
    stream_names[fd] = Calloc(strlen(p)+1, char);
    strcpy(stream_names[fd], p);
}

void closestream(int *handle)
{
    int fd = *handle;
    stream_modes[fd] = 0;
    /* more important to close file than to not free memory */
    if (close(fd)) error("closing stream failed");
    Free(stream_names[fd]);
}

/* origin is 1 2 3 = "start", "current", "end" */
void seek(int *handle, int *offset, int *origin)
{
    lseek(*handle, (off_t) *offset, (*origin == 1) ? SEEK_SET :
	  (*origin == 2 ? SEEK_CUR : SEEK_END));
}


void readchar(int *handle, int *len, int *pn, char **result)
{
    int i, n = *pn;
    for (i = 0; i < n; i++)
	if (read(*handle, result[i], *len) <= 0) break;
    if (i < n) *pn = i;
}


void readint(int *handle, int *pn, int *psize, int *signd,
	     int *swapbytes, int *result)
{
    int i, n = *pn, size = *psize;
    unsigned short us1;
    short s1;
    signed char c1;
    unsigned char uc1;

    for (i = 0; i < n; i++) {
	switch (size) {
	case 4:
	    if (read(*handle, result+i, size) <= 0) goto enddata;
	    if (*swapbytes) swap(result+i, size);
	    break;
	case 2:
	    if (*signd) {
		if (read(*handle, &s1, size) <= 0) goto enddata;
		if (*swapbytes) swap(&s1, size);
		result[i] = (int) s1;
	    } else {
		if (read(*handle, &us1, size) <= 0) goto enddata;
		if (*swapbytes) swap(&us1, size);
		result[i] = (int) us1;
	    }
	    break;
	case 1:
	    if (*signd) {
		if (read(*handle, &c1, 1) <= 0) goto enddata;
		result[i] = (int) c1;
	    } else {
		if (read(*handle, &uc1, 1) <= 0) goto enddata;
		result[i] = (int) uc1;
	    }
	    break;
	default:
	    error("size must be 1, 2 or 4");
	}
    }
    return;
 enddata:
    *pn = i;
    return;
}

void readfloat(int *handle, int *pn, int *psize, int *fromint,
	       int *swapbytes, double *result)
{
    int i, n = *pn, size = *psize, *p1;
    float f1;
    double d1;
#if SIZEOF_LONG_DOUBLE > 8
    long double e1;
#endif
    unsigned int ui1, *p2;


    if (*fromint) {
	for (i = 0; i < n; i++) {
	    switch (size) {
	    case 4: /* must be unsigned */
		if (read(*handle, &ui1, size) <= 0) goto enddata;
		if (*swapbytes) swap(&ui1, size);
		result[i] = (double) f1;
		break;
	    case 8:
		if (read(*handle, &d1, size) <= 0) goto enddata;
		if (*swapbytes) swap(&d1, size);
/* I suspect these need to be swapped by endian-ness */
		if (*fromint > 1) { /* signed */
		    p1 = (int *) &d1;
		    d1 = (double)*p1 + 4294967296.0 * *(p1+1);
		} else {
		    p2 = (unsigned int *) &d1;
		    d1 = (double)*p2 + 4294967296.0 * *(p2+1);
		}
		result[i] = f1;
		break;
	    }
	}
    }
    for (i = 0; i < n; i++) {
	switch (size) {
	case 4:
	    if (read(*handle, &f1, size) <= 0) goto enddata;
	    if (*swapbytes) swap(&f1, size);
	    result[i] = (double) f1;
	    break;
	case 8:
	    if (read(*handle, &d1, size) <= 0) goto enddata;
	    if (*swapbytes) swap(&d1, size);
	    result[i] = d1;
	    break;
#if SIZEOF_LONG_DOUBLE > 8
	case sizeof(long double):
	    if (read(*handle, &e1, size) <= 0) goto enddata;
	    if (*swapbytes) swap(&e1, size);
	    result[i] = (double) e1;
	    break;
#endif
	default:
	    error("That size is unknown on this machine");
	}
    }
    return;
 enddata:
    *pn = i;
    return;
}

void readlines(int *handle, int *pn, int *bufsize, char **eol,
	       char **result)
{
    int i = 0, inpos = *bufsize, outpos	= 0, ibfs = *bufsize,
	obfs = *bufsize, match = 0, neol;
    char *inbuf, *outbuf, *eolc = *eol, null[2];

    neol = strlen(eolc);
    inbuf = Calloc(ibfs, char);
    if (neol == 0) { /* Handle \0 terminator specially */
	null[0] = 0;
	null[1] = 0;
	eolc = &(null[0]);
	neol = 1;
    }

    outbuf = Calloc(obfs, char);

    while (1) {
	if (inpos >= ibfs) { /* recharge the buffer? */
	    if ((ibfs = read(*handle, inbuf, ibfs)) <= 0) {
		/* fake an end of line marker at the end of file */
		outpos += neol - 1;
		match = neol;
		inbuf[0] = '\0';
	    }
	    inpos = 0;
	}
	if (inbuf[inpos] == eolc[match]) match++;
	else match = 0;

	if (outpos >= obfs) { /* not at eol yet, buffer not big enough */
	    obfs *= 2;
	    outbuf = Realloc(outbuf, obfs, char);
	}
	outbuf[outpos++] = inbuf[inpos++];
	if (match >= neol) {	/* matched eol string */
	    outpos -= neol;
	    result[i] = R_alloc(1, outpos + 1);
	    memcpy(result[i], outbuf, outpos);
	    result[i][outpos] = '\0';
	    if ( (ibfs > 0) | (outpos > 0) ) i++;
	    if ( (i >= *pn) | (ibfs <= 0) ) {  /* all done! */
		*pn = i;
		Free(outbuf);
		if (ibfs > 0) lseek(*handle, inpos - ibfs, SEEK_CUR);
		break;
	    }
	    outpos = 0;
	    match = 0;
	}
    }
    Free(inbuf);
}

void writechar(int *handle, int *pn, int *asciiz, char **data)
{
    int i, n=*pn;

    for (i = 0; i < n; i++)
	write(*handle, data[i], strlen(data[i]) + *asciiz);
}


void writefloat(int *handle, int *pn, int *psize, int *toint,
		int *swapbytes, double *data)
{
    int i, n = *pn, size = *psize;
    float f1;
    double d1;
#if SIZEOF_LONG_DOUBLE > 8
    long double e1;
#endif

    if (*toint) error("not yet implemented");
    /* and I don't see how to do so portably on a 32-bit machine */
    for (i = 0; i < n; i++) {
	switch (size) {
	case 4:
	    f1 = (float) data[i];
	    if (*swapbytes) swap(&f1, size);
	    write(*handle, &f1, size);
	    break;
	case 8:
	    d1 = data[i];
	    if (*swapbytes) swap(&d1, size);
	    write(*handle, &d1, size);
	    break;
#if SIZEOF_LONG_DOUBLE > 8
	case sizeof(long double):
	    e1 = (long double) data[i];
	    if (*swapbytes) swap(&d1, size);
	    write(*handle, &e1, size);
	    break;
#endif
	default:
	    error("That size is unknown on this machine");
	}
    }
}


void writeint(int *handle, int *pn, int *psize, int *swapbytes, int *data)
{
    int i, n = *pn, size = *psize;
    int i1;
    short s1;
    signed char c1;

    for (i = 0; i < n; i++) {
	switch (size) {
	case 4:
	    i1 = data[i];
	    if (*swapbytes) swap(&i1, size);
	    write(*handle, &i1, size);
	    break;
	case 2:
	    s1 = (short) data[i];
	    if (*swapbytes) swap(&s1, size);
	    write(*handle, &s1, size);
	    break;
	case 1:
	    c1 = (signed char) data[i];
	    write(*handle, &c1, size);
	    break;
	}
    }
}


/* stream info is

filename
mode
position
size

*/
void streaminfo(int *count, int *handle, char **filename, int *mode,
		int *position, int *size)
{
    int i, n = *count;
    struct stat fs;

    for (i = 0; i < n; i++) {
	mode[i] = stream_modes[handle[i]] - 100;
	if (mode[i] >= 0) {
	    strcpy(filename[i], stream_names[handle[i]]);
	    position[i] = lseek(handle[i], 0L, SEEK_CUR);
	    fstat(handle[i], &fs);
	    size[i] = fs.st_size;
	} else {
	    strcpy(filename[i], "NA");
	    position[i] = size[i] = NA_INTEGER;
	}
    }
}

void Rtruncate(int *handle, int *size)
{
    if (stream_modes[*handle] < 102)
	error("stream is not open in write mode");
    *size = lseek(*handle, 0, SEEK_CUR);
#ifdef HAVE_FTRUNCATE
    if(ftruncate(*handle, *size))
	error("file truncation failed");
#else
#  ifdef WIN32
    if(chsize(*handle, *size))
	error("file truncation failed");
#  else
    error("Unavailable on this platform");
#  endif
#endif
}

void streamcount(int *count)
{
    int i, cnt = 0;

    for (i = 0; i < OPEN_MAX; i++)
	if (stream_modes[i] > 0) cnt ++;
    *count = cnt;
}

void getstreams(int *len, int * result)
{
    int i, j=0;

    for (i = 0; i < OPEN_MAX; i++)
	if (stream_modes[i] > 0) result[j++] = i;
}

void closeallstreams()
{
    int i;

    for (i = 0; i < OPEN_MAX; i++)
	if (stream_modes[i] > 0) close(i);
}

void copystream(int *h1, int *h2, int *nbytes)
{
    char *buf;
    int nin;

    buf = Calloc(*nbytes, char);
    nin = read(*h1, buf, *nbytes);
    if (nin < 0) error("stream read error");
    write(*h2, buf, nin);
    *nbytes = nin;
    Free(buf);
}
