public class VarDecl {
  public static void main( String [] args ) {
    int a = 1;
    System.out.print("impure");
    int b, c, d = 16;
    System.out.print("impure");
    boolean e = (boolean) a;
    System.out.print("impure");
    double f = 10.0;
    System.out.print("impure");
    final int g = 13;

    for ( int h = 0; h < 100; h++ ) {
      System.out.print("impure");
      int i = h, z = 0;
      System.out.print("impure");
      i = 1;
      System.out.print("impure");

      if ( e ) {
        System.out.print("impure");
        int[][] j = {{9, 3, 1}, {3,2,1}};
        System.out.print("impure");
        do {
          System.out.print("impure");
          int[] k = new int[] {1, 2, 3};
          System.out.print("impure");
        } while ( true );
      }
    }

    for ( int n, m = 0 ; ; ) {
      System.out.print("impure");
    }
  }
}