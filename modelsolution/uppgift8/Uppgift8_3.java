
import java.util.Scanner;

public class Uppgift8_3{

  public static void main(String[] args) {

    boolean ok = false;
    double exchange = 0;
    double crowns = 0;
    Scanner sc = new Scanner(System.in);

    //checks that no number is 0
    //uses nextDouble, boolean ok
    //normalization needed: ok == false
    while(!ok){
      System.out.println("Insert exchange course: ");
      exchange = sc.nextDouble();

      if(exchange != 0){
        ok = true;
      }
    }
    ok = false;

    while(!ok){
      System.out.println("Insert crowns: ");
      crowns = sc.nextDouble();

      if(crowns != 0){
        ok = true;
      }
    }
    System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
    sc.close();
  }
}
