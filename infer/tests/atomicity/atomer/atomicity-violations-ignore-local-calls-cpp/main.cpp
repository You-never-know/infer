// Author: Dominik Harmim <iharmim@fit.vut.cz>

#include <mutex>
#include <stdlib.h>
#include <pthread.h>


class Foo
{
public:
	void f1(int i) {}
	void f2(int i) {}
};


class LocalCalls
{
private:
	std::mutex lock;

	Foo foo;


public:
	void test1()
	{
		lock.lock(); // {f1, f2}
		foo.f1(5); foo.f2(42);
		lock.unlock();
	}


	void testNoLocal1()
	{
		foo.f1(5); foo.f2(42); // (f1, f2)
	}


	void testNoLocal2(Foo foo)
	{
		foo.f1(5); foo.f2(42); // (f1, f2)
	}


	void testLocal()
	{
		Foo foo;
		foo.f1(5); foo.f2(42);
	}
};


pthread_mutex_t lock;

typedef int Bar;
Bar bar;


void f1(Bar b, int i) {}
void f2(Bar b, int i) {}


void test1()
{
	pthread_mutex_lock(&lock); // {f1, f2}
	f1(bar, 5); f2(bar, 42);
	pthread_mutex_unlock(&lock);
}


void testNoLocal1()
{
	f1(bar, 5); f2(bar, 42); // (f1, f2)
}


void testNoLocal2(Bar bar)
{
	f1(bar, 5); f2(bar, 42); // (f1, f2)
}


void testLocal()
{
	Bar bar;
	f1(bar, 5); f2(bar, 42);
}


int main()
{
	LocalCalls localCalls;
	Foo foo;

	localCalls.test1();
	localCalls.testNoLocal1();
	localCalls.testNoLocal2(foo);
	localCalls.testLocal();

	if (pthread_mutex_init(&lock, NULL)) return 1;

	test1();
	testNoLocal1();
	testNoLocal2(bar);
	testLocal();

	pthread_mutex_destroy(&lock);

	return 0;
}
