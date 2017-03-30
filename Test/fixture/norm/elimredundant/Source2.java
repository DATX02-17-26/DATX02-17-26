public class C{
  public static void main(String[] args) {
    { int a; }

    for ( int b = 0; b < 3; b++ ) { int x = 0; }
    for ( int b = 0; b < 3; b++ ) { b = 1; }

    if ( true ) { int x = 0; }
    if ( true ) { System.out.println("if"); }

    if ( true ) { int x = 0; }
    else { int y = 0; }
    if ( true ) { System.out.println("if"); }
    else { int y = 0; }
    if ( true ) { int x = 0; }
    else { System.out.println("else"); }
    if ( true ) { System.out.println("if"); }
    else { System.out.println("else"); }

    do { int x = 0; } while ( true );
    do { System.out.println("do"); } while ( true );

    while ( true ) { int x = 0; }
    while ( true ) { System.out.println("while"); }

    switch ( 3 ) {
      case 3: {
        System.out.println("case 3");
      }
      default: {
        System.out.println("default");
      }
    }
  }
}