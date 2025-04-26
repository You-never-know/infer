#include <urcu/urcu-memb.h>
#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include "rcu_example.h"

TData *ptr = NULL;
int counter = 0;
pthread_spinlock_t mutex;

bool random_bool_50() {
    return rand() % 2 == 0;
}

void sleep_random(int interval) {
    srand(time(NULL) ^ getpid());
    int tmp = rand() % (interval + 1);
    usleep(tmp * 1000);
}

void increment_a(TData *pt) {
    pt->a += 1;
}

void decrement_b(TData *pt) {
    pt->b -= 1;
}

void change_char(TData *pt) {
    pt->c += 1;
}

void updater() {
    TData *new = malloc(sizeof(TData));

    pthread_spin_lock(&mutex);
    TData *old = rcu_dereference(ptr);

    *new = *old;
    new->a *= 2;
    new->b *= 4;
    new->c += 1;

    rcu_assign_pointer(ptr, new);
    pthread_spin_unlock(&mutex);

    urcu_memb_synchronize_rcu();
    free(old);
}

void logic1(TData *pt) {
    increment_a(pt);
    change_char(pt);
}

void logic2(TData *pt) {
    TData * pt2 = rcu_dereference(pt);
    increment_a(pt);
}

void reader() {
    urcu_memb_register_thread();

    urcu_memb_read_lock();
    TData *pt1 = rcu_dereference(ptr);
    increment_a(pt1);
    decrement_b(pt1);
    change_char(pt1);
    urcu_memb_read_unlock();

    // Simulated atomicity violation (outside RCU protection)
    decrement_b(pt1);

    if (random_bool_50()) {
        logic1(pt1);
    } else {
        logic2(pt1);
    }

    urcu_memb_unregister_thread();
}

void *summon_reader(void *nop) {
    reader();
    return nop;
}

void *summon_updater(void *nop) {
    updater();
    return nop;
}

void *create_readers(void *nop) {
    pthread_t id[15] = {0};

    for (int j = 0; j < 15; j++) {
        pthread_create(&id[j], NULL, summon_reader, NULL);
        sleep_random(30);
    }

    for (int j = 0; j < 15; j++) {
        pthread_join(id[j], NULL);
    }

    return nop;
}

void *create_updaters(void *nop) {
    pthread_t id[6] = {0};

    for (int j = 0; j < 6; j++) {
        sleep_random(10);
        pthread_create(&id[j], NULL, summon_updater, NULL);
    }

    for (int j = 0; j < 6; j++) {
        pthread_join(id[j], NULL);
    }

    return nop;
}

int main() {
    pthread_spin_init(&mutex, PTHREAD_PROCESS_SHARED);
    urcu_memb_init();

    ptr = malloc(sizeof(TData));
    ptr->a = 1;
    ptr->b = 1;
    ptr->c = 'a';

    pthread_t id1, id2;
    pthread_create(&id1, NULL, create_readers, NULL);
    pthread_create(&id2, NULL, create_updaters, NULL);

    pthread_join(id1, NULL);
    pthread_join(id2, NULL);

    pthread_spin_destroy(&mutex);
    free(ptr);

    return 0;
}
