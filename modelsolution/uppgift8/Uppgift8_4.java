
import java.util.Scanner;

public class Uppgift8_4{

  public static void main(String[] args) {

    boolean ok = false;
    float exchange = 0;
    float crowns = 0;
    Scanner sc = new Scanner(System.in);

    //checks that no number is 0
    //uses nextFloat, boolean ok
    //normalization needed: ok == false
    while(!ok){
      System.out.println("Insert exchange course: ");
      exchange = sc.nextFloat();

      if(exchange != 0){
        ok = true;
      }
    }
    ok = false;

    while(!ok){
      System.out.println("Insert crowns: ");
      crowns = sc.nextFloat();

      if(crowns != 0){
        ok = true;
      }
    }
    System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
    sc.close();
  }
}
