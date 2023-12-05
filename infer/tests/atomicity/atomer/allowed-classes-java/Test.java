// Author: Dominik Harmim <iharmim@fit.vut.cz>

import java.util.ArrayList;


class Foo
{
	public void f() {}
}


class AllowedClasses
{
	private Foo foo = new Foo();

	private ArrayList<Integer> l = new ArrayList<>();


	private void g() {}
	private void h() {}


	synchronized public void test() // {f, clear, add}
	{
		g(); foo.f(); h(); l.clear(); l.add(42);
	}


	public void test_violations()
	{
		foo.f(); l.clear(); l.add(42); // (clear, add)
	}
}


public class Test
{
	public static void main(String[] args)
	{
		AllowedClasses allowedClasses = new AllowedClasses();

		allowedClasses.test();
		allowedClasses.test_violations();
	}
}
