import static java.lang.System.out;
public class group14
{
  public static void main (String[] arg)
  {
    double piCounter = 0;
    for (int i = 0 ; i < 500 ; i++)
    {
      if (i % 2 == 0)
        piCounter = piCounter + (double) 1 / (2 * i + 1);
      else
        piCounter = piCounter - (double) 1 / (2 * i + 1);
    }
    double pi = 4 * piCounter;
    out.println(pi);
  }
}