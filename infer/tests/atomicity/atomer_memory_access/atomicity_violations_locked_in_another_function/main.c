#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>

#define NUM_THREADS 4

int shared_resource = 0;
pthread_mutex_t resource_lock;

void access_shared_resource(int thread_id) {
    // This function assumes caller has already locked!
    shared_resource += thread_id * 2;
}

void safe_update(int thread_id) {
    pthread_mutex_lock(&resource_lock);
    access_shared_resource(thread_id);
    pthread_mutex_unlock(&resource_lock);
}

void* thread_function(void* arg) {
    int id = *(int*)arg;

    for (int i = 0; i < 5000; i++) {
        safe_update(id);
    }

    return NULL;
}

int main() {
    pthread_t threads[NUM_THREADS];
    int ids[NUM_THREADS];

    if (pthread_mutex_init(&resource_lock, NULL) != 0) {
        perror("Failed to initialize mutex");
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < NUM_THREADS; i++) {
        ids[i] = i + 1;
        if (pthread_create(&threads[i], NULL, thread_function, &ids[i]) != 0) {
            perror("Failed to create thread");
            exit(EXIT_FAILURE);
        }
    }

    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }

    pthread_mutex_destroy(&resource_lock);

    printf("Final shared_resource value: %d\n", shared_resource);

    return 0;
}
