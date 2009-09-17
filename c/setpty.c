#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <stdlib.h>

int set_noecho(int fd)
{
	/* borrowed from SBCL, which borrowed from detachtty's detachtty.c,
	 * in turn borrowed from APUE. example code found at
	 * http://www.yendor.com/programming/unix/apue/pty/main.c
	 */
	struct termios  stermios;

	if (tcgetattr(fd, &stermios) < 0) return 0;

	stermios.c_lflag &= ~(  ECHO | /* ECHOE |  ECHOK | */  ECHONL);
	stermios.c_oflag |= (ONLCR);
	/* in contrast to the code in SBCL, we don't want CR except where
	 * user code asks for the line to be cleared: */
	stermios.c_oflag &= ~(ONLCR);
	stermios.c_iflag &= ~(BRKINT);
	stermios.c_iflag |= (ICANON|ICRNL);

	stermios.c_cc[VERASE]=0177;
	if (tcsetattr(fd, TCSANOW, &stermios) < 0) return 0;
	return 1;
}

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
	set_noecho(0);

	execvp(argv[2], argv + 2);
	perror("exec");
}
