public class orderInFor2 
{
  public static int foo()
  {
    int b = 0;
    int a = 0;
    for(int i = 0; i < 100; i++)
    {
      b += i - 1;
      a += i + 1;
    }
    return a;
  }
}
