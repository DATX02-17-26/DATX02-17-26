
import java.util.Scanner;

public class Uppgift8_8{

  public static void main(String[] args) {

    boolean ok = false;
    float exchange = 0;
    float crowns = 0;
    Scanner sc = new Scanner(System.in);

    //uses nextFloat, boolean ok
    //normalization needed: ok == false
    while(!ok){
      System.out.println("Insert exchange course: ");
      exchange = sc.nextFloat();
    }
    ok = false;

    while(!ok){
      System.out.println("Insert crowns: ");
      crowns = sc.nextFloat();
    }
    System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
    sc.close();
  }
}
