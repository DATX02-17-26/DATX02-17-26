
import java.util.Scanner;

public class Uppgift8_11{

  public static void main(String[] args) {

    double exchange = 0;
    double crowns = 0;
    Scanner sc = new Scanner(System.in);

    //uses nextDouble
    //normalization needed: ok == false
    System.out.println("Insert exchange course: ");
    exchange = sc.nextDouble();

    if(exchange != 0){
        System.out.println("Insert crowns: ");
        crowns = sc.nextDouble();
        if(crowns != 0){
          System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
        }
      }
      sc.close();
    }
}
