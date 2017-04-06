public class Uppg12a_6
{
  public static void main (String[] args)
  {
    double summa = 0;
    double pi;
    for (int i = 0 ; i < 1000 ; i += 2)
    {
      if (i % 4 == 0)
        summa = summa + 1.0 / (i + 1);
      else
        summa = summa - 1.0 / (i + 1);
    }
    pi = summa*4;
    System.out.println(pi);
  }
}
