import javax.swing.JOptionPane;

public class Uppgift8_10{

  public static void main(String[] args) {

    String input1 = "";
    String input2 = "";
    float exchange = 0;
    float crowns = 0;
    while(input1.length() <= 0){
      input1 = JOptionPane.showInputDialog("Insert exchange course: ");
      if(input1 == null){
        System.exit(0);
      }
    }
    exchange = Float.parseFloat(input1);


    while(input2.length() <= 0){
      input2 = JOptionPane.showInputDialog("Insert crowns: ");
      if(input2 == null){
        System.exit(0);
      }
    }
    crowns = Float.parseFloat(input2);

    if(crowns != 0 && exchange != 0){
      JOptionPane.showMessageDialog(null, "Output is: " + crowns*exchange);
    }
    else{
      JOptionPane.showMessageDialog(null, "Please gief valid input!");
    }
  }
}
