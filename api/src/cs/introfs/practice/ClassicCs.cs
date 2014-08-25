using System;

namespace Introfs.Practice {

/// <summary>Classic (CSharp) class.</summary>
public class ClassicCs {
	private static readonly log4net.ILog log = 
		log4net.LogManager.GetLogger("prac");
	
	public static long FactLp(long n)
	{
		long acc = 1L;
		log.Info("FactLp()");
		
		for (long i = n; i > 1L; --i)
			acc *= i;
		return acc;
	}
	
	private static long FactIter(long n, long acc)
	{
		return n > 1L ? FactIter(n - 1, acc * n) : acc;
	}
	public static long FactI(long n)
	{
		return FactIter(n, 1L);
	}
	
	public static float ExptLp(float b, float n)
	{
		float acc = 1.0f;
		
		for (float i = n; 0.0f < i; --i)
			acc *= b;
		return acc;
	}
	
	private static float ExptIter(float b, float n, float acc)
	{
		return n > 0.0f ? ExptIter(b, n - 1.0f, acc * b) : acc;
	}
	public static float ExptI(float b, float n)
	{
		return ExptIter(b, n, 1.0f);
	}
	
	/// <summary>Main entry point.</summary>
	/// <param name="args">An array</param>
	/// <returns>The exit code.</returns>
	public static int Main(string[] args)
	{
		long n = 5L;
		Console.Write("Fact({0}) : {1}\n", n, FactI(n));
		return 0;
	}
}

}
