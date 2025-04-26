#ifndef RCU_EXAMPLE_H
#define RCU_EXAMPLE_H

#include <urcu/urcu-memb.h>
#include <pthread.h>

// Shared data structure
typedef struct data {
    int a;
    int b;
    char c;
} TData;

// Shared variables
extern TData *ptr;
extern pthread_spinlock_t mutex;

// Shared memory operations
void increment_a(TData *pt);
void decrement_b(TData *pt);
void change_char(TData *pt);

// Thread worker functions
void *summon_reader(void *nop);
void *summon_updater(void *nop);
void *create_readers(void *nop);
void *create_updaters(void *nop);

// Reader and updater logic
void reader();
void updater();

// Sleep helper
void sleep_random(int interval);

#endif // RCU_EXAMPLE_H
