import java.util.Scanner;

public class C
{
  public static void main (String[] args)
  {
    double pi = 0;
    double num = 1;
    int den = 1;
    for (int i = 0 ; i < 500 ; i++)
    {
      pi += num / den;
      num *= -1;
      den += 2;
    }
    System.out.println((pi * 4));
  }
}
