public class C
{
  public static void main(String[] args)
  {
    {
      int i, j, k;
      i = j + (k + i);
    }
    {
      int i, j, k;
      i = (j + k) * (k + j);
    }
    {
      int i, j, k;
      i = (j*j*(k + 1)*k + j)*k;
    }
    {
      int i;
      i = i*1;
    }
    {
      int j;
      j = j+0;
    }
  }
}
