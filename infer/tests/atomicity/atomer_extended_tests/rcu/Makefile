CC = gcc
CFLAGS = -std=gnu99 -Wall -Wextra

.PHONY: clean all

all: rcu_example

rcu_example: rcu_example.o
	$(CC) $(CFLAGS) $< -pthread -lurcu-memb -o $@

rcu_example.o: rcu_example.c rcu_example.h
	$(CC) $(CFLAGS) -c $< -o $@

clean: 
	rm -f *.o
	rm rcu_example


