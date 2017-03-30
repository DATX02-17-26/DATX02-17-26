
import java.util.Scanner;

public class Uppgift8_5{

  public static void main(String[] args) {

    boolean ok = false;
    double exchange = 0;
    double crowns = 0;
    String input;
    Scanner sc = new Scanner(System.in);

    //uses nextLine, parseDouble, boolean ok
    //normalization needed: ok == false
    while(!ok){
      System.out.println("Insert exchange course: ");
      input = sc.nextLine();

      if(input.length() > 0){
        exchange = Double.parseDouble(input);
        ok = true;
      }
    }
    ok = false;

    while(!ok){
      System.out.println("Insert crowns: ");
      input = sc.nextLine();

      if(input.length() > 0){
        crowns = Double.parseDouble(input);
        ok = true;
      }
    }
    System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
    sc.close();
  }
}
