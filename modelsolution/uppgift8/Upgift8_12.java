
import java.util.Scanner;

public class Uppgift8_11{

  public static void main(String[] args) {

    float exchange = 0;
    float crowns = 0;
    Scanner sc = new Scanner(System.in);

    //uses nextDouble
    //normalization needed: ok == false
    System.out.println("Insert exchange course: ");
    exchange = sc.nextFloat();

    if(exchange != 0){
        System.out.println("Insert crowns: ");
        crowns = sc.nextFloat();
        if(crowns != 0){
          System.out.println("Output is: " + String.format("%.2f", crowns/exchange));
        }
      }
      sc.close();
    }
}
