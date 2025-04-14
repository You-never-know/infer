// Author: Daniel Marek <xmarek72@vutbr.cz>

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

// Dummy functions
void f1() { printf("f1\n"); }
void f2() { printf("f2\n"); }
void f3() { printf("f3\n"); }
void f4() { printf("f4\n"); }
void f5() { printf("f5\n"); }
void f6() { printf("f6\n"); }
void reset() {}
void reset_correct() {}

// Dummy condition function
int random_condition() {
    return 5 > 2;
}

int random_condition_under_lock() {
   return (rand() % 100) > 50;  // returns true ~50% of the time
}

// 40% violation: (f1, f2) — 4 violations, 6 correct
void forty_percent_violation() {
    // 4 violations
    f1(); f2(); reset();
    if (random_condition()) { f1(); f2(); reset(); }
    f1(); f2(); reset();
    f1(); f2(); reset();

    // 6 correct under one lock
    pthread_mutex_lock(&lock);
    f1(); f2(); reset_correct();
    if (random_condition_under_lock()) {
        f1(); f2(); reset_correct();
    }
    f1(); f2(); reset_correct();
    f1(); f2(); reset_correct();
    f1(); f2(); reset_correct();
    f1(); f2(); reset_correct();
    pthread_mutex_unlock(&lock);
}

// 50% violation: (f2, f3) — 5 violations, 5 correct
void fifty_percent_violation() {
    f2(); f3(); reset();
    f2(); f3(); reset();
    if (random_condition()) {
        f2(); f3(); reset();
        f2(); f3(); reset();
    }
    f2(); f3(); reset();

    pthread_mutex_lock(&lock);
    f2(); f3(); reset_correct();
    f2(); f3(); reset_correct();
    if (random_condition_under_lock()) {
        f2(); f3(); reset_correct();
    }
    f2(); f3(); reset_correct();
    f2(); f3(); reset_correct();
    pthread_mutex_unlock(&lock);
}

// 70% violation: (f3, f4) — 7 violations, 3 correct
void seventy_percent_violation() {
    f3(); f4(); reset();
    f3(); f4(); reset();
    f3(); f4(); reset();
    if (random_condition()) { f3(); f4(); reset(); }
    f3(); f4(); reset();
    f3(); f4(); reset();
    f3(); f4(); reset();

    pthread_mutex_lock(&lock);
    f3(); f4(); reset_correct();
    if (random_condition_under_lock()) {
        f3(); f4(); reset_correct();
        f3(); f4(); reset_correct();
    }
    pthread_mutex_unlock(&lock);
}

// 80% violation: (f4, f5) — 8 violations, 2 correct
void eighty_percent_violation() {
    f4(); f5(); reset();
    f4(); f5(); reset();
    f4(); f5(); reset();
    if (random_condition()) {
        f4(); f5(); reset();
        f4(); f5(); reset();
        f4(); f5(); reset();
        f4(); f5(); reset();
        f4(); f5(); reset();
    }

    pthread_mutex_lock(&lock);
    f4(); f5(); reset_correct();
    if (random_condition_under_lock()) { f4(); f5(); reset_correct(); }
    pthread_mutex_unlock(&lock);
}

// 90% violation: (f5, f6) — 9 violations, 1 correct
void ninety_percent_violation() {
    f5(); f6(); reset();
    f5(); f6(); reset();
    f5(); f6(); reset();
    f5(); f6(); reset();
    if (random_condition()) { f5(); f6(); reset(); }
    f5(); f6(); reset();
    f5(); f6(); reset();
    f5(); f6(); reset();
    f5(); f6(); reset();

    pthread_mutex_lock(&lock);
    f5(); f6(); reset_correct();
    pthread_mutex_unlock(&lock);
}

int main() {
    //srand(42);  // Optional: seed for repeatability
    forty_percent_violation();
    fifty_percent_violation();
    seventy_percent_violation();
    eighty_percent_violation();
    ninety_percent_violation();
    return 0;
}
