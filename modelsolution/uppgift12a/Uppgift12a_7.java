public class Uppgift12a_7 {
   public static void main(String[] args) {

      double pi = 0;
      double denominator = 1;

      for (int i = 0; i < 500; i++) {

         if (i % 2 == 0) {
            pi = pi + (1 / denominator);
         } else {
            pi = pi - (1 / denominator);
         }
         denominator = denominator + 2;
      }
      System.out.println((pi*4));
   }
 }
