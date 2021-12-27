CC = gcc
CFLAGS = -std=gnu99 -Wall -Wextra
LFLAGS = -pthread -lurcu-memb

.PHONY: clean all

all: rcu_2_locks rcu_2_unlocks rcu_deadlock rcu_unprotected_read rcu_intraprocedural rcu_deadlock2

rcu_intraprocedural: rcu_intraprocedural.o
	$(CC) $(CFLAGS) $< $(LFLAGS) -o $@

rcu_2_locks: rcu_2_locks.o 
	$(CC) $(CFLAGS) $< $(LFLAGS) -o $@
	
rcu_2_unlocks: rcu_2_unlocks.o 
	$(CC) $(CFLAGS) $< $(LFLAGS) -o $@
	
rcu_deadlock: rcu_deadlock.o 
	$(CC) $(CFLAGS) $< $(LFLAGS) -o $@

rcu_deadlock2: rcu_deadlock2.o
	$(CC) $(CFLAGS) $< $(LFLAGS) -o $@
	
rcu_unprotected_read: rcu_unprotected_read.o 
	$(CC) $(CFLAGS) $< $(LFLAGS) -o $@

%.o: %.c rcu.h
	$(CC) $(CFLAGS) -c $< -o $@

clean: 
	rm -f *.o
	rm rcu_2_locks
	rm rcu_2_unlocks
	rm rcu_deadlock
	rm rcu_unprotected_read
	rm rcu_intraprocedural
	rm rcu_deadlock2


