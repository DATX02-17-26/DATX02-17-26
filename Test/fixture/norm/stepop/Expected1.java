public class C {
  public static void dummy(int i) {}
  public static void main (String[] args) {
    int i = 0;

    // SExpr:
    i = i + 1;
    i = i - 1;
    i = i + 1;
    i = i - 1;

    // For:
    for ( int j = 0, k = 0; j < 10;
          j = j + 1, k = 0, i = i + 1, j = j - 1, i = i - 1) {}

    // In expressions:
    dummy( ((i = i + 1) - 1)
         * ((i = i - 1) + 1)
         * (i = i + 1)
         * (i = i - 1)
        );
  }
}