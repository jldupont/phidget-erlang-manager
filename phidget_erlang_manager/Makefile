PREFIX=/usr
DEBUG?=n
DMALLOC?=n
CROSSCOMPILE?=n

CC=gcc


INCLUDES := -I/usr/include
LIBS := -L/usr/lib/happycoders -lerl_interface -lei -lphidget21 -lsocket -lnsl

phidget_erl_manager :=	src/main.c     \
						src/manager.c  \
						src/logger.c   \
						src/queuer.c   \
						src/server.c

all: phidget_erl_manager.o

phidget_erl_manager.o: 
	$(CC) -o phidget_erl_manager.o $(INCLUDES) $(LIBS) $(phidget_erl_manager)

clean: 
	-rm *.a 
	-rm *~ 
	-rm *.so 
	-rm *.o

