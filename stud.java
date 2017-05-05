import java.util.Scanner;

public class stud 
{
  public static void main(String[] argv)
  {
    Scanner in = new Scanner(System.in);
    int n   = in.nextInt();
    int sum = 0;
    /*
    if (n == 12)
      System.out.print("hej\n");
    */
    int i = 0;
    while(i < n)
    {
      sum += in.nextInt();
      i++;
    }
    System.out.print(sum);
  }
}
