import java.util.Scanner;

public class sumNumbers1
{
  public static void main(String[] argv)
  {
    Scanner in = new Scanner(System.in);
    int n   = in.nextInt();
    if(n > 10)
      System.out.print(n);
    int sum = 0;
    for(int i = 0; i < n; i++)
      sum += in.nextInt();
    System.out.print(sum);
  }
}
