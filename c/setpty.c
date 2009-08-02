#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <stdlib.h>

int
main(int argc, char **argv)
{
	char *ptyname;
	int fd;
	
	if (argc < 3) return 1;

	/* grab ourselves a new process group */
	setsid();

	/* get rid of the old tty, if any */
        fd = open("/dev/tty", O_RDWR, 0);
        if (fd >= 0) {
            ioctl(fd, TIOCNOTTY, 0);
            close(fd);
        }

	/* here comes the new one */
	ptyname = argv[1];
	close(0);
	if (open(ptyname, O_RDWR) != 0) perror("open");
	dup2(0, 1);
	dup2(0, 2);

	execvp(argv[2], argv + 2);
	perror("exec");
}
