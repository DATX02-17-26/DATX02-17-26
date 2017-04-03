
import java.util.Scanner;

public class Uppgift8_1{

  public static void main(String[] args) {

    boolean ok = false;
    double exchange = 0;
    double crowns = 0;
    String input;
    Scanner sc = new Scanner(System.in);

    //checks that no number is 0
    //uses nextLine, parseDouble, boolean ok
    //normalization needed: ok == false
    while(!ok){
      System.out.println("Insert exchange course: ");
      input = sc.nextLine();

      if(input.length() > 0){
        exchange = Double.parseDouble(input);

        if(exchange != 0){
          ok = true;
        }
      }
    }
    ok = false;

    while(!ok){
      System.out.println("Insert crowns: ");
      input = sc.nextLine();

      if(input.length() > 0){
        crowns = Double.parseDouble(input);

        if(crowns != 0){
          ok = true;
        }
      }
    }
    System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
    sc.close();
  }
}
