PREFIX=/usr
DEBUG?=n
DMALLOC?=n
CROSSCOMPILE?=n

CC=gcc


# -L/usr/lib/erlang/lib/erl_interface-3.5.7/lib  \

#LIBS := -L/usr/lib/happycoders -lsocket -lnsl -pthread -lphidget21 -lei
LIBS := -L/usr/lib/happycoders -lsocket -lnsl -pthread -lphidget21 -lei

phidget_erl_manager :=	src/main.c     \
						src/manager.c  \
						src/logger.c   \
						src/queuer.c   \
						src/server.c

all: phidget_erl_manager

phidget_erl_manager: 
	$(CC) -v -Wall $(phidget_erl_manager) $(LIBS) -o phidget_erl_manager   

clean: 
	-rm phidget_erl_manager

