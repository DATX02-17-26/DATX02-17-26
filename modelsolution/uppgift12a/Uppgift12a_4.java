public class Uppgift12a_4 {
   public static void main (String[] arg)
   {
      double sum = 0;
      int num = 1;
      int den = 1;
      for (int i = 1 ; i <= 500 ; i++)
      {
         sum += 1.0 / (num * den);
         num += 2;
         den *= -1;
      }
      double pi = 4 * sum;
      System.out.println(pi);
   }
 }
