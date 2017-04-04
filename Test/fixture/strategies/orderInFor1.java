public class orderInFor1 
{
  public static int foo()
  {
    int a = 0;
    int b = 0;
    for(int i = 0; i < 100; i++)
    {
      a += i + 1;
      b += i - 1;
    }
    return a;
  }
}
