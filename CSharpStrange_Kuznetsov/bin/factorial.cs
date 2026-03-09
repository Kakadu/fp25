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
        return Factorial(5);
    }
}