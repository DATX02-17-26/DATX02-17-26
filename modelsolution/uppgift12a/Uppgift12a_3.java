public class Uppgift12a_3{
  public static void main(String[]args){

    double pi = 0;
    double quotient;
    for (int i = 1; i <= 500; i++) {
      quotient = (1.0 / ((i * 2) - 1));
      if (i%2 != 0){
        pi = pi + quotient;
      }
      else{
        pi = pi - quotient;
      }
    }
    System.out.println(pi*4);
  }
}
