#ifndef RCU
#define RCU

#include <sys/types.h>
#include <unistd.h>

void * create_readers(void * nop);
void * create_updaters(void * nop); 

void updater();
void reader(int i);

void lock_for_me();
void unlock_for_me();

void * summon_reader(void *nop);
void * summon_updater(void *nop);
void sleep_random(int interval);

#endif

