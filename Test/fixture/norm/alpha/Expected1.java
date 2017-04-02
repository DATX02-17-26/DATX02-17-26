import java.util.Scanner;

public class Class1
{
  public static void method1 (String[] var1)
  {
    double var2 = 0;
    double var3 = 1;
    int var4 = 1;
    for (int var5 = 0 ; var5 < 500 ; var5++)
    {
      var2 += var3 / var4;
      var3 *= -1;
      var4 += 2;
    }
    System.out.println((var2 * 4));
  }
}
