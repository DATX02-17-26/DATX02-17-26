public class Uppgift12a_2
{
  public static void main (String[] args)
  {
    double sum = 0;
    double num = 1;
    double den = 1;
    for (int i = 0 ; i <= 499 ; i++)
    {
      sum = sum + num / den;
      num = num * -1;
      den = den + 2;
    }
    double pi = sum * 4;
    System.out.println(pi);
  }
}
