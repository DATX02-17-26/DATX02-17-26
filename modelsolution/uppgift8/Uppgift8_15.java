import javax.swing.JOptionPane;

public class Uppgift8_12{

  public static void main(String[] args) {

    String input = "";
    double exchange = 0;
    double crowns = 0;

    input = JOptionPane.showInputDialog("Insert exchange course: ");
    if(input == null || input.equals("")){
      System.exit(0);
    }
    exchange = Double.parseDouble(input);
    input = JOptionPane.showInputDialog("Insert crowns: ");
    if(input == null || input.equals("")){
      System.exit(0);
    }
    crowns = Double.parseDouble(input);
    JOptionPane.showMessageDialog(null, "Output is: " + crowns/exchange);
  }
}
