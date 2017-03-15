public class mod5
{
  public static void main(String[] argv)
  {
    int sum = 0, i = 0;
    while(i <= 100)
    {
      if(i % 2 == 0)
        sum += i;
      i++;
    }
    System.out.println(sum);
  }
}
