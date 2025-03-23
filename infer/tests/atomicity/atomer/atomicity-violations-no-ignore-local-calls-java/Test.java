// Author: Dominik Harmim <iharmim@fit.vut.cz>

class Foo
{
	public void f1(int i) {}
	public void f2(int i) {}
}


class LocalCalls
{
	private Foo foo = new Foo();


	public void test1()
	{
		synchronized(this) { // {f1, f2}
			foo.f1(5); foo.f2(42);
		}
	}


	public void testNoLocal1()
	{
		foo.f1(5); foo.f2(42); // (f1, f2)
	}


	public void testNoLocal2(Foo foo)
	{
		foo.f1(5); foo.f2(42); // (f1, f2)
	}


	public void testLocal()
	{
		Foo foo = new Foo();
		foo.f1(5); foo.f2(42); // (f1, f2)
	}
}


public class Test
{
	public static void main(String[] args)
	{
		LocalCalls localCalls = new LocalCalls();

		localCalls.test1();
		localCalls.testNoLocal1();
		localCalls.testNoLocal2(new Foo());
		localCalls.testLocal();
	}
}
