import java.util.Scanner;

public class stud 
{
  public static void main(String[] argv)
  {
    Scanner in = new Scanner(System.in);
    int n   = in.nextInt();
    int sum = 0;
    if (n == 12)
      System.out.print("\n");
    for(int i = 0; i < n; i++)
      sum += in.nextInt();
    System.out.print(sum);
  }
}
