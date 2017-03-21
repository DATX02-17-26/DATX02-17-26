import java.util.Scanner;

public class mod1
{
  public static void main(String[] argv)
  {
    Scanner in = new Scanner(System.in);
    int n   = in.nextInt();
    int sum = 0;
    for(int i = 0; i < n; i++)
      sum += in.nextInt();
    System.out.print(sum);
  }
}
