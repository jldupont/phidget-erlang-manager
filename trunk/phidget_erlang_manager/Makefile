PREFIX=/usr
DEBUG?=n
DMALLOC?=n
CROSSCOMPILE?=n

CC=gcc


LIBS := -L/usr/lib/happycoders -lsocket -lnsl -pthread -lphidget21 -lei

phidget_erl_manager :=	src/main.c     \
						src/manager.c  \
						src/logger.c   \
						src/queuer.c   \
						src/server.c   \
						src/signals.c  \
						src/daemon.c

all: phidget_erl_manager

phidget_erl_manager: 
	$(CC) -Wall $(phidget_erl_manager) $(LIBS) -o phidget_erl_manager   

clean: 
	-rm phidget_erl_manager

