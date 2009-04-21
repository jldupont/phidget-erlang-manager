PREFIX=/usr
DEBUG?=n
DMALLOC?=n
CROSSCOMPILE?=n

CC=gcc


INCLUDES := /usr/include
LIBS := -lerl_interface -lphidget21 -lei -lsocket

phidget_erl_manager := src/main.c src/manager.c src/logger.c src/queuer.c src/server.c

all: phidget_erl_manager.o

phidget_erl_manager.o: 
	$(CC) -o phidget_erl_manager.o -I$(INCLUDES) $(LIBS) $(phidget_erl_manager)

clean: 
	-rm *.a 
	-rm *~ 
	-rm *.so 
	-rm *.o

