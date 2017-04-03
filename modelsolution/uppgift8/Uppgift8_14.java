import javax.swing.JOptionPane;

public class Uppgift8_13{

  public static void main(String[] args) {

    String input = "";
    float exchange = 0;
    float crowns = 0;

    input = JOptionPane.showInputDialog("Insert exchange course: ");
    if(input != null && !input.equals("")){
      exchange = Float.parseFloat(input);
      input = JOptionPane.showInputDialog("Insert crowns: ");
      if(input != null && !input.equals("")){
        crowns = Float.parseFloat(input);
        JOptionPane.showMessageDialog(null, "Output is: " + crowns/exchange);
      }
    }
  }
}
