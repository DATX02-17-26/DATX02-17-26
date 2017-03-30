import java.util.Scanner;

public class Uppgift8_6{

  public static void main(String[] args) {

    boolean ok = false;
    float exchange = 0;
    float crowns = 0;
    String input;
    Scanner sc = new Scanner(System.in);

    //uses nextLine, parseFloat, boolean ok
    //normalization needed: ok == false
    while(!ok){
      System.out.println("Insert exchange course: ");
      input = sc.nextLine();

      if(input.length() > 0){
        exchange = Float.parseFloat(input);
        ok = true;
      }
    }
    ok = false;

    while(!ok){
      System.out.println("Insert crowns: ");
      input = sc.nextLine();

      if(input.length() > 0){
        crowns = Float.parseFloat(input);
        ok = true;
      }
    }
    System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
    sc.close();
  }
}
