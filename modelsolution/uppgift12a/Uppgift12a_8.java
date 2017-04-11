public class Uppgift12a_2
{
  public static void main (String[] args)
  {
    double pi = 0;
    double num = 1;
    double den = 0;
    for (int i = 0 ; i < 500 ; i++)
    {
      pi += num / (1 + den);
      num *= -1;
      den += 2;
    }
    pi = 4 * pi;
    System.out.println(pi);
  }
}
