public class C{

  public static void main(String[] args) {
    int sum = 0;
    {
        int lol;
      for(int i = 0; i < 3; i++){
        sum = sum + i;

        for(int j = 0; j < 2; j++){
          sum = sum + j;
        }

      }
    }
  }
}
