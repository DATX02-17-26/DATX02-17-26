public class C{

  public static void main(String[] args) {
    int sum = 0;
      {
      int lol;
      {
        int i = 0;
        while(i < 3){
          sum = sum + i;
          {
            int j = 0;
            while(j < 2){
              sum = sum + j;
              j++;
            }
          }
          i++;
        }
      }
    }
  }
}
