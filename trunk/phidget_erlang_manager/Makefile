PREFIX=/usr
DEBUG?=n
DMALLOC?=n
CROSSCOMPILE?=n

CC=gcc


INCLUDES := /usr/include
LIBS := erl_interface

phidget_erl_manager := src/main.c

all: phidget_erl_manager.o

phidget_erl_manager.o: 
	$(CC) -o phidget_erl_manager.o -I$(INCLUDES) -L$(LIBS) $(phidget_erl_manager)

clean: 
	-rm *.a *~ *.so *.o

