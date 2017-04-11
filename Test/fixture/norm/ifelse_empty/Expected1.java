public class C {
  public static void keep() {}
  public static void main(String[] args) {
    // preliminaries:
    int i = 0;
    int j = 0;
    boolean b = true;
    int[] arr;

    // normalized into:
    if ( !b ) keep();
    if ( b ) keep();
    i++; j++;
    if ( b ) { i++; } else { j++; }
    arr = new int[] { 1 }; j++;
  }
}