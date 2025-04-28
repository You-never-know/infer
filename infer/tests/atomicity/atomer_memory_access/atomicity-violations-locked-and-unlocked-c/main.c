#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>

#define NUM_THREADS 4

int shared_data = 0;
pthread_mutex_t lock;

void* update_shared_data(void* arg) {
    int id = *(int*)arg;

    for (int i = 0; i < 5000; i++) {
        if (i % 2 == 0) {
            // Access protected by a lock
            pthread_mutex_lock(&lock);
            shared_data += id;
            pthread_mutex_unlock(&lock);
        } else {
            // Access without any lock
            shared_data -= id;
        }
    }
    return NULL;
}

int main() {
    pthread_t threads[NUM_THREADS];
    int ids[NUM_THREADS];

    if (pthread_mutex_init(&lock, NULL) != 0) {
        perror("Failed to initialize mutex");
        exit(EXIT_FAILURE);
    }

    // Create threads
    for (int i = 0; i < NUM_THREADS; i++) {
        ids[i] = i + 1; // Thread IDs start at 1
        if (pthread_create(&threads[i], NULL, update_shared_data, &ids[i]) != 0) {
            perror("Failed to create thread");
            exit(EXIT_FAILURE);
        }
    }

    // Join threads
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }

    pthread_mutex_destroy(&lock);

    printf("Final shared_data value: %d\n", shared_data);

    return 0;
}
