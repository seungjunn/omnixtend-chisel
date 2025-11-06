CC = gcc
CFLAGS = -Wall -O2

all: send_ethernet

send_ethernet: send_ethernet.c
	$(CC) $(CFLAGS) -o send_ethernet send_ethernet.c

clean:
	rm -f send_ethernet

.PHONY: all clean

