#include "chibicc.h"

char *xstrndup(const char *s, size_t n) {
  char *buf = calloc(1, n + 1);
  memcpy(buf, s, n);
  buf[n] = '\0';
  return buf;
}

char *xdirname(const char *path) {
  if (!path || !*path)
    return strdup(".");

  const char *slash = strrchr(path, '/');
  if (!slash)
    return strdup(".");
  if (slash == path)
    return strdup("/");
  return xstrndup(path, slash - path);
}

char *xbasename(const char *path) {
  if (!path || !*path)
    return strdup(".");

  const char *slash = strrchr(path, '/');
  if (!slash)
    return strdup(path);
  if (*(slash + 1) == '\0')
    return strdup("/");
  return strdup(slash + 1);
}

int xwait(int *status) {
  return waitpid(-1, status, 0);
}

void run_subprocess_go(char **argv) {
  if (fork() == 0) {
    execvp(argv[0], argv);
    fprintf(stderr, "exec failed: %s: %s\n", argv[0], strerror(errno));
    _exit(1);
  }

  int status;
  while (xwait(&status) > 0)
    ;
  if (status != 0)
    exit(1);
}
