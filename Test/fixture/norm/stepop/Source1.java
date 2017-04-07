public class C {
  public static void dummy(int i) {}
  public static void main (String[] args) {
    int i = 0;

    // SExpr:
    i++;
    i--;
    ++i;
    --i;

    // For:
    for ( int j = 0, k = 0; j < 10;
          j++, k = 0, ++i, --j, i--) {}

    // In expressions:
    dummy( i++
         * i--
         * ++i
         * --i
        );
  }
}