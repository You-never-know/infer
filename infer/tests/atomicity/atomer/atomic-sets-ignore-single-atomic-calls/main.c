// Author: Dominik Harmim <iharmim@fit.vut.cz>

#include <stdlib.h>
#include <pthread.h>


pthread_mutex_t lock;


void f1(void) {}
void f2(void) {}
void f3(void) {}
void f4(void) {}
void f5(void) {}
void f6(void) {}
void f7(void) {}
void f8(void) {}
void f9(void) {}
void f10(void) {}
void f11(void) {}
void f12(void) {}
void x(void) {}
void ff(void) { f7(); f8(); }


void test1(void)
{
	pthread_mutex_lock(&lock); // {f1, f2}
	f1(); f2();
	pthread_mutex_unlock(&lock);

	x();

	pthread_mutex_lock(&lock);
	f3(); f4();
	pthread_mutex_unlock(&lock);
}


void test2(void)
{
	pthread_mutex_lock(&lock); // {f1, f2}
	f1(); f2();
	pthread_mutex_unlock(&lock);

	x();

	pthread_mutex_lock(&lock); // {f5, f6}
	f5(); f6(); f5(); f6();
	pthread_mutex_unlock(&lock);
}


void test_nested(void)
{
	pthread_mutex_lock(&lock); // {f1, f7, f8}
	ff(); f1();
	pthread_mutex_unlock(&lock);

	x();

	pthread_mutex_lock(&lock); // {f2, f7, f8}
	f7(); f2(); f8();
	pthread_mutex_unlock(&lock);
}


void test_iteration(void)
{
	int c;

	pthread_mutex_lock(&lock);
	while (c)
	{
		f9(); f10();
	}
	pthread_mutex_unlock(&lock);
}


void test_selection(void)
{
	int a, b;

	if (a > b)
	{
		pthread_mutex_lock(&lock); // {f11, f12}
		f11(); f12();
		pthread_mutex_unlock(&lock);
	}
	else
	{
		pthread_mutex_lock(&lock); // {f11, f12}
		f11(); f12();
		pthread_mutex_unlock(&lock);
	}
}


int main(void)
{
	if (pthread_mutex_init(&lock, NULL)) return 1;

	test1();
	test2();
	test_nested();
	test_iteration();
	test_selection();

	pthread_mutex_destroy(&lock);

	return 0;
}
