public class VarDecl
{
  public static void main (String[] args)
  {
    boolean e;
    int a;
    int b;
    int c;
    int d;
    int h;
    int n;
    int m;
    int i;
    int z;
    double f;
    int[] k;
    int[][] j;
    a = 1;
    System.out.print("impure");
    d = 16;
    System.out.print("impure");
    e = (boolean) a;
    System.out.print("impure");
    f = 10.0;
    System.out.print("impure");
    final int g = 13;
    h = 0;
    for (; h < 100 ; h++)
    {
      System.out.print("impure");
      i = h;
      z = 0;
      System.out.print("impure");
      i = 1;
      System.out.print("impure");
      if (e)
      {
        System.out.print("impure");
        j = new int[][] {
                          { 9, 3, 1 },
                          { 3, 2, 1 },
                        };
        System.out.print("impure");
        do
        {
          System.out.print("impure");
          k = new int[] {
                          1,
                          2,
                          3,
                        };
          System.out.print("impure");
        } while (true);
      }
    }
    m = 0;
    for (; ;)
    {
      System.out.print("impure");
    }
  }
}