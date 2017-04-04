public class Uppgift12a_5
{
  public static void main (String[] args)
  {
    double summa = 0;
    for (int i = 0 ; i < 500 ; i++)
    {
      if (i % 2 == 0)
        summa = summa + 1.0 / (i + 1);
      else
        summa = summa - 1.0 / (i + 1);
    }
    double pi = summa * 4;
    System.out.println(pi);
  }
}
