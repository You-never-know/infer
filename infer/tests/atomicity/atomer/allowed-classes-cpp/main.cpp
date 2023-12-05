// Author: Dominik Harmim <iharmim@fit.vut.cz>

#include <mutex>
#include <vector>


void f() {}


class Foo
{
public:
	void g() {}
};


class AllowedClasses
{
private:
	std::mutex lock;

	Foo foo;

	std::vector<int> v;


	void h() {}


public:
	void test()
	{
		lock.lock(); // {h, clear, push_back}
		f(); foo.g(); h(); v.clear(); v.push_back(42);
		lock.unlock();
	}


	void test_violations()
	{
		h(); v.clear(); v.push_back(42); // (clear, push_back)
	}
};


int main()
{
	AllowedClasses allowedClasses;

	allowedClasses.test();
	allowedClasses.test_violations();

	return 0;
}
