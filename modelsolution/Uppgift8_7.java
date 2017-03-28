
import java.util.Scanner;

public class Uppgift8_7{

  public static void main(String[] args) {

    boolean ok = false;
    double exchange = 0;
    double crowns = 0;
    Scanner sc = new Scanner(System.in);

    //uses nextDouble, boolean ok
    //normalization needed: ok == false
    while(!ok){
      System.out.println("Insert exchange course: ");
      exchange = sc.nextDouble();
    }
    ok = false;

    while(!ok){
      System.out.println("Insert crowns: ");
      crowns = sc.nextDouble();
    }
    System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
    sc.close();
  }
}
