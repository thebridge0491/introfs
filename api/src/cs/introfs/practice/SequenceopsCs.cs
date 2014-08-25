using System;
//using SysCollGen = System.Collections.Generic;
using System.Collections.Generic;

namespace Introfs.Practice {

/// <summary>Sequenceops (CSharp) class.</summary>
public class SequenceopsCs {
	private static readonly log4net.ILog log = 
		log4net.LogManager.GetLogger("prac");
	
	public static void SwapItems<T>(int a, int b, List<T> lst)
	{
		T swap = lst[a];
		lst.Insert(a, lst[b]); lst.RemoveAt(a + 1);
		lst.Insert(b, swap); lst.RemoveAt(b + 1);
	}
	
	public static List<T> CopyOf<T>(List<T> lst)
	{
		var newLst = new List<T>();
	
		for (var i = 0; lst.Count > i; ++i)
			newLst.Add(lst[i]);
		return newLst;
	}
	
	public static int IndexOfLp<T>(T data, List<T> lst)
	{
		log.Info("IndexOfLp()");
		
		for (var i = 0; lst.Count > i; ++i)
			if (lst[i].Equals(data))
				return i;
		return -1;
	}
	
	public static void ReverseLp<T>(List<T> lst)
	{
		for (int i = 0, j = lst.Count - 1; j > i; ++i,--j)
			SwapItems<T>(i, (int)j, lst);
	}
	
	/// <summary>Main entry point.</summary>
	/// <param name="args">An array</param>
	/// <returns>The exit code.</returns>
	public static int Main(string[] args)
	{
		int n = 3;
		int[] arr1 = {2, 1, 0, 4, 3};
		var lst = new List<int>(arr1);
		Console.Write("IndexOf({0}, [{1}]) : {2}\n", n, 
			String.Join(", ", lst), IndexOfLp(n, lst));
		return 0;
	}
}

}
