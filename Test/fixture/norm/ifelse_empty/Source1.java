public class C {
  public static void keep() {}
  public static void main(String[] args) {
    // preliminaries:
    int i = 0;
    int j = 0;
    boolean b = true;
    int[] arr;

    // for normalization:
    if ( b ) ; else keep();
    if ( b ) keep(); else ;
    if ( (i++ - j++) == 0 ) ;
    if ( (b ? i++ : j++) == 0 ) ;
    if ( (arr = new int[] { 1 })[j++] == 1 ) ; else ;
  }
}