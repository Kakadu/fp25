public class Program {
      public static int Fibonacci(int n) 
      {
        if (n <= 1)
    {
            return n;
        }
        return Fibonacci(n - 1) + Fibonacci (n - 2);
      }
      public static int Main() {
        System.Console.WriteLine(Fibonacci   (6  ));
        return 0;
 }
    } 
