#include <R.h>
#include <Rdefines.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>


#define LINECHUNK_LENGTH 1024

static char *get_line(FILE *f)
{
  char *line, *l;
  size_t chunk_length, num_chars_total = 0, num_chars_chunk;

  chunk_length = LINECHUNK_LENGTH;
  line = (char *) S_alloc(chunk_length, sizeof(char));
  if (line == NULL)
  {
    error("xpipe/get_line: S_alloc returned NULL (out of memory)");
  }
  l = line;
  while (fgets(l, chunk_length - num_chars_total, f))
  {
    num_chars_chunk = strlen(l);
    if (num_chars_chunk == 0)
    {
      *l = '\0';
      break;
    }
    num_chars_total += num_chars_chunk;
    if (line[num_chars_total - 1] == '\n')
    {
      line[num_chars_total - 1] = '\0';
      break;
    }
    l = S_realloc(line, chunk_length * 2, chunk_length, sizeof(char));
    if (l == NULL)
    {
      error("xpipe/get_line: S_realloc returned NULL (out of memory)");
    }
    chunk_length *= 2;
    line = l;
    l = line + num_chars_total;
  }
  if (num_chars_total == 0)
  {
    return (NULL);
  }
  /* REprintf("get_line: returning %lu chars: \"%s\"\n", (unsigned long) num_chars_total, line); */
  return (line);
}


static char **xpipe_lines(const char *cmd, SEXP *input)
{
  int fd[2];
  pid_t pid;

  /* REprintf("xpipe_lines: starting\n"); */
  if (pipe(fd))
  {
    REprintf("pipe() failed");
  }
/*
  else
  {
    REprintf("got pipe: read = %d, write = %d\n", fd[0], fd[1]);
  }
*/
  fflush(stdout);
  fflush(stderr);
  pid = fork();
  if (pid)
  {
    FILE *f = fdopen(fd[0], "r");
    long num_lines = 0;
    char **output = NULL, **o;

    close(fd[1]);
    while (!feof(f))
    {
      if (output == NULL)
      {
	o = (char **) S_alloc(1, sizeof(char *));
      }
      else
      {
	o = (char **) S_realloc((char *) output, num_lines + 2, num_lines + 1, sizeof(char *));
      }
      if (o == NULL)
      {
	error("xpipe/xpipe_lines: S_[re]alloc returned NULL (out of memory)");
      }
      output = o;
      output[num_lines++] = get_line(f);
      output[num_lines] = NULL;
    }
    fclose(f);
    return(output);
  }
  else
  {
    FILE *f;

    R_len_t l = LENGTH(*input), i;

    close(fd[0]);
    dup2(fd[1], fileno(stdout));
    f = popen(cmd, "w");
    /* FIXME: if this popen() fails, the parent process should get
       this somehow */
    if (f == NULL)
    {
      exit(EXIT_FAILURE);
    }
    for (i = 0; i < l; i++)
    {
      /* REprintf("writing %s\n", CHAR(STRING_ELT(*input, i))); */
      fprintf(f, "%s\n", CHAR(STRING_ELT(*input, i)));
    }
    pclose(f);
  }
  exit(EXIT_SUCCESS);
}


typedef enum
{
  RFULLPIPE_LINES,
  RFULLPIPE_NUMERIC,
  RFULLPIPE_DATAFRAME,
  RFULLPIPE_RAW,
  RFULLPIPE_0
}  RFULLPIPE_RETURNTYPE;


typedef struct
{
  RFULLPIPE_RETURNTYPE returntype;
  const char *name;
}
RETURNTYPETABLE_ENTRY;


static RETURNTYPETABLE_ENTRY returntypetable[] =
{
  {RFULLPIPE_LINES, "lines"},
  {RFULLPIPE_NUMERIC, "numeric"},
  {RFULLPIPE_DATAFRAME, "data.frame"},
  {RFULLPIPE_RAW, "raw"},
  {RFULLPIPE_0, ""}
};


static RFULLPIPE_RETURNTYPE get_returntype(const char *name)
{
  RETURNTYPETABLE_ENTRY *r = returntypetable;

  while (r->returntype != RFULLPIPE_0)
  {
    if (!strcmp(name, r->name))
    {
      return (r->returntype);
    }
  }
  warning("unknowwn return type");
  return (RFULLPIPE_0);
}


SEXP Rxpipe_muell(SEXP x)
{
  SEXP muell = R_NilValue;

  /* muell = allocSExp(NILSXP); */
  return (muell);
}


SEXP Rxpipe_xpipe(SEXP cmd, SEXP input, SEXP returntype_exp)
{
  SEXP output;
  size_t l, i;
  char **char_output;
  RFULLPIPE_RETURNTYPE returntype = get_returntype(CHAR(STRING_ELT(returntype_exp, 0)));
  if (returntype == RFULLPIPE_0)
  {
    error("unknown return type");
  }
  if (returntype != RFULLPIPE_LINES)
  {
    error("unsupported return type");
  }

/*
  printf("cmd: \"%s\"\n", CHAR(STRING_ELT(cmd, 0)));
  printf("input: %ld lines\n", LENGTH(input));
  for (i = 0; i < LENGTH(input); i++)
  {
    printf("  %s\n", CHAR(STRING_ELT(input, i)));
  }
*/
  char_output = xpipe_lines(CHAR(STRING_ELT(cmd, 0)), &input);
  if (char_output == NULL)
  {
    /* FIXME: This should also happen if the popen() call of the child failed,
       but this is not captured by the parent */
    /* REprintf("xpipe: no output -- returning NULL\n"); */
    output = allocSExp(NILSXP);
    return (output);
  }
  for (l = 0; char_output[l]; l++)
    ;
  PROTECT(output = allocVector(STRSXP, l));
  for (i = 0; i < l; i++)
  {
    /* REprintf("%lu: %s\n", (unsigned long) i, char_output[i]); */
    SET_STRING_ELT(output, i, mkChar(char_output[i]));
  }
  UNPROTECT(1);
  /* Notice: I assume that all S_(re)alloced stuff is freed at
     this return */
  return (output);
}
