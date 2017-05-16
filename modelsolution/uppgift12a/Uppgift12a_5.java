public class Uppgift12a_5
{
  public static void main (String[] args)
  {
    double summa = 0;
    for (int i = 0 ; i < 500 ; i++)
    {
      if (i % 2 == 0)
        summa += 1 / (i * 2.0 + 1);
      else
        summa -= 1 / (i * 2.0 + 1);
    }
    summa *= 4;
    System.out.println(summa);
  }
}
