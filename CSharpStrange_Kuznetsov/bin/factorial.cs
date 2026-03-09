public class Program
{
    public static int Factorial(int n)
    {
        if (n == 0)
        {
            return 1;
        }
        else
        {
            return n * Factorial(n - 1);
        }
    }

    public static int Main()
    {
        System.Console.WriteLine(Factorial (5));
        return 0;
    }
}