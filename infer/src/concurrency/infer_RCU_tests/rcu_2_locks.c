#include <urcu/urcu-memb.h>
#include <urcu/rculist.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include "rcu.h"
#include <pthread.h>
#include <stdio.h>


typedef struct data {

	int a;
	int b;
	char c;

} TData;

int counter = 0;

pthread_spinlock_t mutex;

TData *  ptr = NULL;

int main() {

	pthread_spin_init(&mutex, PTHREAD_PROCESS_SHARED);

	ptr = malloc(sizeof(TData));

	ptr->a = 1;
	ptr->b = 1;
	ptr->c = 'a'; 

	urcu_memb_init();

	pthread_t id = 0;

	pthread_t id2 = 0;

	pthread_create(&id, NULL, create_readers , NULL);

	pthread_create(&id2, NULL, create_updaters , NULL);

	pthread_join(id2, NULL);

	pthread_join(id, NULL); 
	
	pthread_spin_destroy(&mutex);

	printf("Last pointer freed\n");
	free(ptr);

	return 0;
}


void updater() {


	TData * new = malloc(sizeof(TData));

	pthread_spin_lock(&mutex);

	TData * old = rcu_dereference(ptr);
	//TData * old = ptr;
	new->a = 2*(old->a);
	new->b = 4*(old->b);
	new->c = old->c + 1;

	rcu_assign_pointer(ptr, new);
	//ptr = new;

	fprintf(stdout, "\nUpdated to | 1. value %d | 2. value %d | Char: %c\n \n", new->a, new->b, new->c);
	printf("--------------------------------------------------------------------------------------------\n");
	fflush(stdout);

	//printf("New pointer %p\n", new);

	//rcu_assign_pointer(ptr, new);

	sleep_random(50);

	pthread_spin_unlock(&mutex);

	urcu_memb_synchronize_rcu();

	free(old);
	printf("Old pointer freed\n");
	fflush(stdout);

}

void reader(int i) {

	urcu_memb_register_thread();

	urcu_memb_read_lock();
	urcu_memb_read_lock();
	urcu_memb_read_lock();

	//printf("Reader pointer %p\n", ptr);

	TData * pt = rcu_dereference(ptr);
	
	sleep_random(100);

	fprintf(stdout, "Process: %d | 1. value %d | 2. value %d | Char: %c\n", i, pt->a, pt->b, pt->c);
	fflush(stdout);	
	printf("----------------------------------------------------------------------------------------\n");
	
	fflush(stdout);

	urcu_memb_read_unlock();

	//sleep_random(100);
	//fprintf(stdout, "\nForbiden: Process: %d | Char %c\n", i, pt->c);

	urcu_memb_unregister_thread();

}

void * create_readers(void * nop) {


	pthread_t id[15] = {0};

	for (int j = 0; j< 15; j++) {

		pthread_create(&id[j], NULL, summon_reader , NULL);
		sleep_random(30);

	}

	for (int j = 0; j< 15; j++) {

		pthread_join(id[j], NULL); 

	}
	
	return nop;

}


void * create_updaters(void * nop) {


	pthread_t id[6] = {0};

	for (int j = 0; j < 6; j++) {

		sleep_random(10);

		pthread_create(&id[j], NULL, summon_updater , NULL);

	}

	for (int j = 0; j< 6; j++) {

		pthread_join(id[j], NULL); 

	}
	
	return nop;
}

void * summon_reader(void *nop) {

	counter++;
	reader(counter);

	return nop;

} 


void * summon_updater(void *nop) {

	updater();

	return nop;
}


void sleep_random(int interval) {

	srand(time(NULL)); // random seed
	int tmp = rand() % (interval + 1);
	if (tmp < 0) {
		tmp *=-1;
	}

	usleep(tmp *1000);

}
