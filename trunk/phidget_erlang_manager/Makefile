PREFIX=/usr
DEBUG?=n
DMALLOC?=n
CROSSCOMPILE?=n

CC=gcc


LIBS := -L/usr/lib/happycoders -lsocket -lnsl -pthread -lphidget21 -lei

phidgetmanager :=	src/main.c     \
						src/manager.c  \
						src/logger.c   \
						src/queuer.c   \
						src/server.c   \
						src/signals.c  \
						src/daemon.c

all: phidgetmanager

phidgetmanager: 
	$(CC) -Wall $(phidget_erl_manager) $(LIBS) -o phidgetmanager 

clean: 
	-rm phidgetmanager

